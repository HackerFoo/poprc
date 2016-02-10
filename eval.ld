SECTIONS
{
  .tests :
  {
      PROVIDE(__tests_start = .);
      KEEP(SORT(*)(.tests))
      PROVIDE(__tests_end = .);
  }
}

INSERT AFTER .dtors;
