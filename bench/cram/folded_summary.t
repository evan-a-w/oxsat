folded_summary.sh summarizes folded stacks (frame;frame;leaf COUNT) into a
flat profile: self% counts samples where the frame is the leaf, tot% counts
samples where the frame appears anywhere in the stack (once per stack).

  $ sh ../folded_summary.sh <<EOF
  > a;b;c 10
  > a;b;d 5
  > a;c 3
  > x 1
  > EOF
    self%    tot%       self      total  function
   68.42%  68.42%         13         13  c
   26.32%  26.32%          5          5  d
    5.26%   5.26%          1          1  x
    0.00%  78.95%          0         15  b
    0.00%  94.74%          0         18  a
