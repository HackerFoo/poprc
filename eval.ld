
SECTIONS
{
  . = 0x10000;
  .text : { *(.text) }
  .test :
  {
      __test_start = .;
      *(.test)
      __test_end = .;
  }
  . = 0x8000000;
  .data : { *(.data) }
  .bss : { *(.bss) }
}
