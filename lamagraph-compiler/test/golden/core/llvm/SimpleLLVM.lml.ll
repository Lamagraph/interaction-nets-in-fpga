target triple = "unknown-unknown-unknown-unknown-"
define i64 @"\22f\22 :| []"(i64 %a0, i64 %a1) {
  %r0 = add i64 %a0, %a1
  %r1 = add i64 %a1, %r0
  ret i64 %r1
}