SECTIONS
{
  .tests :
  {
      PROVIDE(__tests_start = .);
      KEEP(*(.tests))
      PROVIDE(__tests_end = .);
  }
}

INSERT AFTER .dtors;
