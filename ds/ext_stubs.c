#define CAML_INTERNALS

#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

#include <stdint.h>
#include <stdlib.h>
#include <string.h>

typedef struct ext_slot {
  value value;
  struct ext_pool *pool;
  struct ext_slot *next_free;
  int allocated;
} ext_slot;

typedef struct ext_chunk {
  ext_slot *slots;
  char *objects;
  size_t len;
  struct ext_chunk *next;
} ext_chunk;

typedef struct ext_pool {
  size_t chunk_size;
  ext_chunk *chunks;
  ext_slot *free_list;
  value default_value;
  int is_immediate;
  mlsize_t allocated_wosize;
  mlsize_t scannable_wosize;
  tag_t tag;
  reserved_t reserved;
  intnat outstanding;
  int destroyed;
} ext_pool;

static value ptr_to_nativeint(void *ptr)
{
  return caml_copy_nativeint((intnat)(uintptr_t)ptr);
}

static void *nativeint_to_ptr(value v)
{
  return (void *)(uintptr_t)Nativeint_val(v);
}

static value ptr_to_int(void *ptr)
{
  uintptr_t raw = (uintptr_t)ptr;
  if ((raw & 1) != 0) {
    caml_failwith("Ext: unaligned pointer");
  }
  return Val_long((intnat)(raw >> 1));
}

static void *int_to_ptr(value v)
{
  return (void *)(uintptr_t)(((uintnat)Long_val(v)) << 1);
}

static mlsize_t scannable_wosize(header_t header)
{
  if (!Scannable_hd(header)) {
    return 0;
  }
#ifdef NATIVE_CODE
  return Scannable_wosize_hd_native(header);
#else
  return Scannable_wosize_hd_byte(header);
#endif
}

static header_t out_of_heap_header(ext_pool *pool)
{
  return Caml_out_of_heap_header_with_reserved(
    pool->allocated_wosize, pool->tag, pool->reserved);
}

static ext_pool *pool_of_value(value v)
{
  ext_pool *pool = (ext_pool *)nativeint_to_ptr(v);
  if (pool == NULL || pool->destroyed) {
    caml_failwith("Ext: pool has been destroyed");
  }
  return pool;
}

static ext_slot *slot_of_value(value v)
{
  ext_slot *slot = (ext_slot *)int_to_ptr(v);
  if (slot == NULL) {
    caml_failwith("Ext: null slot");
  }
  return slot;
}

static void register_roots(value block, mlsize_t scannable_wosize)
{
  for (mlsize_t i = 0; i < scannable_wosize; i++) {
    caml_register_global_root((value *)&Field(block, i));
  }
}

static void remove_roots(value block, mlsize_t scannable_wosize)
{
  for (mlsize_t i = 0; i < scannable_wosize; i++) {
    Field(block, i) = Val_unit;
    caml_remove_global_root((value *)&Field(block, i));
  }
}

static value alloc_external_block(ext_pool *pool)
{
  size_t byte_size = Bsize_wsize(Whsize_wosize(pool->allocated_wosize));
  header_t *header = malloc(byte_size);
  if (header == NULL) {
    caml_raise_out_of_memory();
  }

  *header = out_of_heap_header(pool);
  value block = Val_hp(header);
  for (mlsize_t i = 0; i < pool->allocated_wosize; i++) {
    Field(block, i) = Val_unit;
  }
  register_roots(block, pool->scannable_wosize);
  return block;
}

static void free_external_block(ext_pool *pool, value block)
{
  if (Is_block(block)) {
    remove_roots(block, pool->scannable_wosize);
    free((void *)Hp_val(block));
  }
}

static void check_shape(ext_pool *pool, value source)
{
  if (pool->is_immediate) {
    if (!Is_long(source)) {
      caml_invalid_argument("Ext.set: expected immediate value");
    }
    return;
  }

  if (!Is_block(source)) {
    caml_invalid_argument("Ext.set: expected block value");
  }

  header_t header = Hd_val(source);
  if (Tag_hd(header) != pool->tag
      || Allocated_wosize_hd(header) != pool->allocated_wosize
      || Reserved_hd(header) != pool->reserved
      || scannable_wosize(header) != pool->scannable_wosize) {
    caml_invalid_argument("Ext.set: value shape differs from pool default");
  }
}

static void copy_value_to_slot(ext_pool *pool, ext_slot *slot, value source)
{
  check_shape(pool, source);

  if (pool->is_immediate) {
    slot->value = source;
    return;
  }

  memcpy(Op_val(slot->value), Op_val(source),
         Bsize_wsize(pool->allocated_wosize));
}

static void allocate_chunk(ext_pool *pool)
{
  ext_chunk *chunk = malloc(sizeof(ext_chunk));
  if (chunk == NULL) {
    caml_raise_out_of_memory();
  }

  chunk->len = pool->chunk_size;
  chunk->objects = NULL;
  chunk->slots = calloc(chunk->len, sizeof(ext_slot));
  if (chunk->slots == NULL) {
    free(chunk);
    caml_raise_out_of_memory();
  }

  if (!pool->is_immediate) {
    size_t object_size = Bsize_wsize(Whsize_wosize(pool->allocated_wosize));
    chunk->objects = malloc(chunk->len * object_size);
    if (chunk->objects == NULL) {
      free(chunk->slots);
      free(chunk);
      caml_raise_out_of_memory();
    }

    header_t header = out_of_heap_header(pool);
    for (size_t i = 0; i < chunk->len; i++) {
      header_t *slot_header =
        (header_t *)(chunk->objects + (i * object_size));
      *slot_header = header;
      value block = Val_hp(slot_header);
      for (mlsize_t field = 0; field < pool->allocated_wosize; field++) {
        Field(block, field) = Val_unit;
      }
      register_roots(block, pool->scannable_wosize);
      chunk->slots[i].value = block;
    }
  }

  chunk->next = pool->chunks;
  pool->chunks = chunk;

  for (size_t i = 0; i < chunk->len; i++) {
    ext_slot *slot = &chunk->slots[i];
    slot->pool = pool;
    slot->allocated = 0;
    if (pool->is_immediate) {
      slot->value = pool->default_value;
    }
    slot->next_free = pool->free_list;
    pool->free_list = slot;
  }
}

CAMLprim value oxsat_ext_create_pool(value v_chunk_size, value v_default)
{
  CAMLparam2(v_chunk_size, v_default);
  intnat chunk_size = Long_val(v_chunk_size);
  if (chunk_size <= 0) {
    caml_invalid_argument("Ext.create_pool: chunk_size must be positive");
  }

  ext_pool *pool = malloc(sizeof(ext_pool));
  if (pool == NULL) {
    caml_raise_out_of_memory();
  }

  pool->chunk_size = (size_t)chunk_size;
  pool->chunks = NULL;
  pool->free_list = NULL;
  pool->is_immediate = Is_long(v_default);
  pool->default_value = Val_unit;
  pool->allocated_wosize = 0;
  pool->scannable_wosize = 0;
  pool->tag = 0;
  pool->reserved = 0;
  pool->outstanding = 0;
  pool->destroyed = 0;

  if (pool->is_immediate) {
    pool->default_value = v_default;
  } else {
    header_t header = Hd_val(v_default);
    pool->allocated_wosize = Allocated_wosize_hd(header);
    pool->scannable_wosize = scannable_wosize(header);
    pool->tag = Tag_hd(header);
    pool->reserved = Reserved_hd(header);
    pool->default_value = alloc_external_block(pool);
    copy_value_to_slot(pool, &(ext_slot){ .value = pool->default_value },
                       v_default);
  }

  CAMLreturn(ptr_to_nativeint(pool));
}

CAMLprim value oxsat_ext_pool_outstanding(value v_pool)
{
  CAMLparam1(v_pool);
  ext_pool *pool = pool_of_value(v_pool);
  CAMLreturn(Val_long(pool->outstanding));
}

CAMLprim value oxsat_ext_destroy_pool(value v_pool)
{
  CAMLparam1(v_pool);
  ext_pool *pool = (ext_pool *)nativeint_to_ptr(v_pool);
  if (pool == NULL || pool->destroyed) {
    CAMLreturn(Val_unit);
  }

  pool->destroyed = 1;
  ext_chunk *chunk = pool->chunks;
  while (chunk != NULL) {
    ext_chunk *next = chunk->next;
    if (!pool->is_immediate) {
      for (size_t i = 0; i < chunk->len; i++) {
        remove_roots(chunk->slots[i].value, pool->scannable_wosize);
      }
      free(chunk->objects);
    }
    free(chunk->slots);
    free(chunk);
    chunk = next;
  }
  free_external_block(pool, pool->default_value);
  free(pool);

  CAMLreturn(Val_unit);
}

CAMLprim value oxsat_ext_alloc(value v_pool)
{
  CAMLparam1(v_pool);
  ext_pool *pool = pool_of_value(v_pool);
  if (pool->free_list == NULL) {
    allocate_chunk(pool);
  }

  ext_slot *slot = pool->free_list;
  pool->free_list = slot->next_free;
  slot->next_free = NULL;
  slot->allocated = 1;
  copy_value_to_slot(pool, slot, pool->default_value);
  pool->outstanding++;

  CAMLreturn(ptr_to_int(slot));
}

CAMLprim value oxsat_ext_free(value v_slot)
{
  CAMLparam1(v_slot);
  ext_slot *slot = slot_of_value(v_slot);
  ext_pool *pool = slot->pool;
  if (pool == NULL || pool->destroyed) {
    caml_failwith("Ext: pool has been destroyed");
  }
  if (!slot->allocated) {
    caml_failwith("Ext.free: double free");
  }

  if (!pool->is_immediate) {
    for (mlsize_t i = 0; i < pool->scannable_wosize; i++) {
      Field(slot->value, i) = Val_unit;
    }
  }
  slot->allocated = 0;
  slot->next_free = pool->free_list;
  pool->free_list = slot;
  pool->outstanding--;

  CAMLreturn(Val_unit);
}

CAMLprim value oxsat_ext_get(value v_slot)
{
  CAMLparam1(v_slot);
  ext_slot *slot = slot_of_value(v_slot);
  if (!slot->allocated) {
    caml_failwith("Ext.get: freed slot");
  }
  CAMLreturn(slot->value);
}

CAMLprim value oxsat_ext_is_freed(value v_slot)
{
  CAMLparam1(v_slot);
  ext_slot *slot = slot_of_value(v_slot);
  CAMLreturn(Val_bool(!slot->allocated));
}

CAMLprim value oxsat_ext_set(value v_slot, value v_value)
{
  CAMLparam2(v_slot, v_value);
  ext_slot *slot = slot_of_value(v_slot);
  if (!slot->allocated) {
    caml_failwith("Ext.set: freed slot");
  }
  copy_value_to_slot(slot->pool, slot, v_value);
  CAMLreturn(Val_unit);
}
