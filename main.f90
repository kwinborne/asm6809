!Copyright (c) 2020, Kiyle A. Winborne
!All rights reserved.

!Redistribution and use in source and binary forms, with or without
!modification, are permitted provided that the following conditions are met:
!* Redistributions of source code must retain the above copyright notice, this list of conditions and the following
!* disclaimer. Redistributions in binary form must reproduce the above copyright notice, this
!* list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
!* Neither the name of the <organization> nor the names of its contributors may be used to endorse or promote products
!* derived from this software without specific prior written permission.

! THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED
! WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
! PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL <COPYRIGHT HOLDER> BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
! EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
! LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER
! IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
!SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
program asm6809
    implicit none
    !**************************************
    ! Instruction Set arrays Defined Here
    !**************************************

    character(5):: instruc(120) =  (/"ADC  " , "ADD  " ,"AND  ","ASL  ","LSL  " ,"BCC  ","BHS  ","BCS  ","BLO  " ,"BEQ  ",&
    "BMI  ","BNE  ","BPL  " ,"BRA  ","BSR  " , "CLR  " ,"CMP  ","DEC  ","INC  " ,"JSR  ","LSR  ","PSH  ","PUL  " ,"ROL  ",&
    "ROR  ","RTS  ","SUB  " ,"ASR  ","BGE  " , "BGT  " ,"BHI  ","BIT  ","BLE  " ,"BLT  ","COM  ","DAA  ","EOR  " ,"EXG  ",&
    "JMP  ","LEA  ","MUL  " ,"NEG  ","NOP  " , "RTI  ","SWI  " ,"TFR  ", "TST  ","ABX  ","BRN  ","BVC  ","BVS  " ,"SBC  ",&
    "SEX  ","adc  ","add  " ,"and  ","asl  " , "lsl  ","bcc  " ,"bhs  ", "bcs  ","blo  ","beq  ","bmi  ","bne  " ,"bpl  ",&
    "bra  ","bsr  ","clr  " ,"cmp  ","dec  " , "inc  " ,"jsr  ","lsr  ","psh  " ,"pul  ","rol  ","ror  ","rts  " ,"sub  ",&
    "asr  ","bge  ","bgt  " ,"bhi  ","bit  " , "ble  " ,"blt  ","com  ","daa  " ,"eor  ","exg  ","jmp  ","lea  " ,"mul  ",&
    "neg  ","nop  ", "rti  ","swi  ","tfr  " , "tst  " ,"abx  ","brn  ","bvc  " ,"bvs  ","sbc  ","sex  ","ANDCC" ,"andcc",&
    "ORCC ","orcc ","SYNC " ,"sync ","CWAI " , "cwai " ,"LD   ","ld   ","ST   " ,"st   ","OR   ","or   "/)


    character(128), allocatable :: input_source(:) ! Array used to manipulate source file.
    integer :: line_count
    !*****************************!
    ! Command line argument logic !
    !*****************************!
    character(16) :: args_input, args_output ! Command line arguments. That is, the input and output files.
    integer :: arg_num
    arg_num = iargc() ! Returns the number of arguments at command line.
    call get_command_argument(1, args_input)
    call get_command_argument(2, args_output)
    args_input = trim(args_input) ! Strip blank spaces from the arguments
    args_output = trim(args_output)
    if (arg_num < 2) then ! Check to see if there are input and output file names, if not print a message and terminate.
        print *, "Usage: asm6809 <source> <output>. Type asm6809 --help for more information."
        call exit()
    end if
    call file_read(input_source, args_input,line_count) ! Call subroutine to read source file and store it in input_source.




    call preprocessor(input_source, line_count,instruc)
    contains

    !***************************************************************************************
    ! File read subroutine. Defines size of input_source and handles IO related IO routine.
    ! By using an input buffer, we can "slice" the source code into lines, and store them
    ! in an array to iterate through.
    !***************************************************************************************
    subroutine file_read(input, args, file_size)
        character(128), allocatable, intent(inout) :: input(:)
        integer, intent(out) :: file_size
        character(16), intent(in) :: args ! i.e., command line arguments
        character(128):: buffer
        integer :: i, input_size
        open(10, file=args, action="read")
        do                                  ! Get the number of lines in the file.
            read(10, *, end=10)
            file_size = file_size+1
        end do
10      allocate(character(128)::input(file_size))
        rewind(10)
        do i=1,file_size                    ! Read one buffers full and assign the corresponding index to that line.
        read(10, '(A)', end = 50) buffer    ! End of file points us to label 50, which closes the file.
            input(i) = buffer
        end do
50      close(1)
    end subroutine file_read
    ! ******************************************************************
    ! Preprocessor - Handles Syntax checking and assembler directives
    ! ******************************************************************

    subroutine preprocessor(source,file_size,instruction)
    character(128), allocatable, intent(in) :: source(:)
    character(5), intent(in) :: instruction(120)
    integer, intent(in) :: file_size
    character(128) :: line_buffer
    integer :: i,j, line_counter=0
    logical :: instruc_found = .false., is_label =.false.
    do i=1,file_size-1                  ! Second preprocessor pass. Syntax check.
        line_buffer = source(i)
        line_counter = i
        if(index(line_buffer,';') == 0)then ! If the current line is a comment, skip it.
            if(.not.is_label)then
                do j=1,120
                    instruc_found = (index(line_buffer, trim(instruction(j)))/= 0)
                    if(instruc_found)then
                        exit
                    end if
                    if(.not.instruc_found .and. j==120)then
                        print '(A, I3)', "Syntax error: Instruction not found on line ", line_counter
                        print *, "The instruction entered was: " ,trim(line_buffer)
                        call exit
                    end if
                end do
                cycle
           end if
        end if
    end do
    end subroutine preprocessor
    !*******************************************************************
    ! Parser - Actual translation to ML happens here.
    ! Get a line, check it against a table, and act accordingly.
    !*******************************************************************
    subroutine parser
    end subroutine parser
    !******************************************************************
    ! ... it writes the output to a file.
    !******************************************************************
    subroutine file_write
    end subroutine file_write

end program asm6809
