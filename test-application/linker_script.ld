OUTPUT_FORMAT(elf64-x86-64)
OUTPUT_ARCH(i386:x86-64)

ENTRY(_start)

PHDRS {
    rodata          PT_LOAD     FLAGS(4)    ;
    text            PT_LOAD     FLAGS(1 | 4);
    data            PT_LOAD     FLAGS(2 | 4);
    dynamic         PT_DYNAMIC              ;
    boot_request    0x69B2Ba6E              ;
}

SECTIONS {
    .rodata ALIGN(CONSTANT(COMMONPAGESIZE)) : {
        *(.rodata .rodata.*)
    } :rodata

    .text ALIGN(CONSTANT(COMMONPAGESIZE)) : {
        *(.text .text.*)
    } :text

    .data ALIGN(CONSTANT(COMMONPAGESIZE)) : {
        *(.data .data.*)
    } :data

    .bss : {
        *(.bss .bss.*)
    } :data

    .got : {
        *(.got .got.*)
    } :data

    .dynamic : {
        *(.dynamic .dynamic.*)
    } :dynamic

    .bootloader_request : {
        KEEP(*(.bootloader_request))
    } :boot_request
}
