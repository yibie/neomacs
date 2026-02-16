BEGIN {
  case_prefix = "__NEOVM_CASE__\t";
  case_count = 0;
  allow_empty = ENVIRON["NEOVM_ALLOW_EMPTY_CASE_OUTPUT"] == "1";
}

{
  pos = index($0, case_prefix);
  if (pos > 0) {
    print substr($0, pos + length(case_prefix));
    case_count++;
  }
}

END {
  if (!allow_empty && case_count == 0) {
    print "vm-compat runner emitted no prefixed case lines" > "/dev/stderr";
    exit 2;
  }
}
