       *>**
       *>  ECBLUnit
       *>  Simple Unit Testing for z/OS written in IBM Enterprise COBOL
       *>
       *>  @author Olegs Kunicins
       *>  @license GPL
       *>
       *>  This program is free software; you can redistribute it and/or
       *>  modify it under the terms of the GNU General Public License 
       *>  as published by the Free Software Foundation; either version
       *>  2, or (at your option) any later version.
       *>  
       *>  This program is distributed in the hope that it will be 
       *>  useful, but WITHOUT ANY WARRANTY; without even the implied 
       *>  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
       *>  PURPOSE. See the GNU General Public License for more details.
       *>  
       *>  You should have received a copy of the GNU General Public
       *>  License along with this software; see the file COPYING.
       *>  If not, write to the Free Software Foundation, Inc.,
       *>  59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
       *>**
       
       identification division.
       program-id. ECBLUNIT.
       environment division.
       data division.
       working-storage section.
      * constants
       01 INTRO.
        05 filler pic x(8) value "ECBLUnit".
        05 filler pic x value SPACE.
        05 filler pic x(6) value "1.62.6".
        05 filler pic x value SPACE.
        05 filler pic x(35) value "by Olegs Kunicins and contributors.".
        05 filler pic x(2) value x'15'.
       01 HELP.
        05 filler pic x(13) value "Usage in JCL:".
        05 filler pic x(2) value x'15'.
        05 filler pic x(32) value "//RUN          EXEC PGM=ECBLUNIT".
        05 filler pic x(30) value ",PARM='<testA testB ...>'".

      * assertions
       01 assertions-counter pic 9(3) usage binary external.
       01 summary-pointer usage pointer external.
       01 summary.
        03 assertions-total pic 9(3) usage binary.
        03 failures-total pic 9(3) usage binary.
        03 assertions occurs 0 to 999 depending on assertions-counter.
         05 assertion-status pic x value SPACE.
          88 assertion-failed value "F".
         05 filler pic x value SPACE.
         05 assertion-suite pic x(32).
         05 filler pic x value "#".
         05 assertion-nr pic 9(2).
         05 filler pic x value SPACE.
         05 assertion-name pic x(16).
         05 filler pic x value SPACE.
         05 assertion-expected pic x(32).
         05 filler pic x(4) value " <> ".
         05 assertion-actual pic x(32).

      * handler
       01 ws-err-ptr usage procedure-pointer.
       01 ws-err-msg-ptr usage pointer.
       01 ws-err-fc pic x(12) value low-value.

      * local
       01 assertions-index pic 9(3) usage binary.
       01 first-suite pic x(32).
        88 is-empty value SPACE.
       01 current-time.
        05 hours pic 9(2).
        05 minutes pic 9(2).
        05 seconds pic 9(2).
       01 elapsed-time.
        05 hours pic 9(2).
        05 minutes pic 9(2).
        05 seconds pic 9(2).
       01 test-pointer usage procedure-pointer.
       01 testsuite-name pic x(8) value SPACES.
       01 arg-idx pic 9(3) usage binary value 1.
       01 arg-first pic 9(3) usage binary value 1.
       01 tests-total pic 9(3) usage binary.
       01 skipped-total pic 9(3) usage binary.
       01 errors-total pic 9(3) usage binary external.
       01 ws-argv-ptr pic 9(9) usage binary value 1.
       linkage section.
       01 arg.
        05 arg-len pic 9(3) usage binary.
        05 arg-value pic x(81) value SPACES.
       procedure division using arg.
       ecblu-init section.
           perform ecblu-start.

           add 1 to arg-len.
           move SPACE to arg-value(arg-len:1).
           perform varying arg-idx from 1 by 1 until arg-idx > arg-len
             if arg-value(arg-idx:1) = SPACE
               move arg-value(arg-first:arg-idx - arg-first) 
                    to testsuite-name
               compute arg-first = 1 + arg-idx
               perform ecblu-exec
             end-if
           end-perform.

           perform ecblu-finish.
           stop run.

       ecblu-start section.
           display INTRO.
           if arg-len = 0
              display HELP
              stop run
           end-if.

           *>set ws-err-ptr to entry "ECBLUERR".
           set ws-err-msg-ptr to address of testsuite-name.
           set summary-pointer to address of summary.
           accept elapsed-time from TIME.

       ecblu-exec section.
           add 1 to tests-total.
           set test-pointer to entry testsuite-name.
           *>call "CEEHDLR" using ws-err-ptr, ws-err-msg-ptr, ws-err-fc.
           call test-pointer.
           *>call "CEEHDLU" USING ws-err-ptr, ws-err-msg-ptr, ws-err-fc.

       ecblu-finish section.
           accept current-time from TIME.
           subtract corresponding current-time from elapsed-time.
           
           *> time
           display "Time: " 
             hours of elapsed-time ":" 
             minutes of elapsed-time ":" 
             seconds of elapsed-time.
       
           *> failures
           if failures-total of summary > 0
             display "There was " failures-total of summary 
             " failure(s):"
           end-if.
           move 0 to assertions-index.
           perform until assertions-index >= assertions-total of summary
             add 1 to assertions-index
             if assertion-failed(assertions-index)
               display assertions(assertions-index)
             end-if
           end-perform.
       
           *> report
           if errors-total > 0
             display "EXCEPTIONS!"
             move 1 to RETURN-CODE 
           end-if.
           if failures-total of summary > 0
             display "FAILURES!"
             move 1 to RETURN-CODE 
           end-if.
           if errors-total = 0 and failures-total of summary = 0
             if tests-total > 0 and assertions-total of summary > 0
               display "OK"
             else 
               if tests-total = 0
                 display "No tests found"
               else 
                 display "No assertions found"
               end-if
             end-if
             move 0 to RETURN-CODE 
           end-if.
           display "Tests: " tests-total ", Skipped: " skipped-total.
           display "Assertions: " assertions-total of summary
             ", Failures: " failures-total of summary
             ", Exceptions: " errors-total.
       end program ECBLUNIT.

      * Error handler
       identification division.
       program-id. ECBLUERR.
       data division.
       working-storage section.
       01 errors-total pic 9(3) usage binary external.
       linkage section.
       01 curr-token pic x(12).
       01 token-ptr usage pointer.
       01 rc pic s9(9) usage binary.
       01 new-token pic x(12).
       01 testsuite-name pic x(8).
       procedure division using curr-token, token-ptr, rc, new-token.
           set address of testsuite-name to token-ptr.
           if testsuite-name not = SPACE
             add 1 to errors-total
             display SPACE
             display "There was an error in " testsuite-name
           end-if.
           move 10 to rc.
           move "*OMIT" to new-token.
       end program ECBLUERR.

      * Assert EQ
       identification division.
       program-id. ECBLUEQ.
       data division.
       working-storage section.
       01 assertions-counter pic 9(3) usage binary external.
       01 summary-pointer usage pointer external.
       01 assertions-nr pic 9(2).
       01 comparison pic s9(9) usage binary.
       01 idx pic 9(2) usage binary.
       linkage section.
       01 expected pic x(32).
       01 actual pic x(32). 
       01 summary.
        03 assertions-total pic 9(3) usage binary.
        03 failures-total pic 9(3) usage binary.
        03 assertions occurs 0 to 999 depending on assertions-counter.
         05 assertion-status pic x value SPACE.
          88 assertion-failed value "F".
         05 filler pic x value SPACE.
         05 assertion-suite pic x(32).
         05 filler pic x value "#".
         05 assertion-nr pic 9(2).
         05 filler pic x value SPACE.
         05 assertion-name pic x(16).
         05 filler pic x value SPACE.
         05 assertion-expected pic x(32).
         05 filler pic x(4) value " <> ".
         05 assertion-actual pic x(32).
       procedure division using expected, actual.
           set address of summary to summary-pointer.
           add 1 to assertions-total.
           add 1 to assertions-nr.
           add 1 to assertions-counter.
           move assertions-nr to assertion-nr(assertions-counter).

           move 0 to idx.
           move 0 to comparison.
           perform until idx >= length of actual
               add 1 to idx 
               compute comparison = 
                 function ord(expected(idx:1)) 
                 - function ord(actual(idx:1))
               if comparison not = 0
                   exit perform
               end-if
           end-perform.

           if comparison = 0
               move "." to assertion-status(assertions-counter)
           else
               move "F" to assertion-status(assertions-counter)
               add 1 to failures-total
           end-if.

           move expected to assertion-expected(assertions-counter).
           move actual to assertion-actual(assertions-counter).
       end program ECBLUEQ.

      * Assert NEQ
       identification division.
       program-id. ECBLUNEQ.
       data division.
       working-storage section.
       01 assertions-counter pic 9(3) usage binary external.
       01 summary-pointer usage pointer external.
       01 assertions-nr pic 9(2).
       01 comparison pic s9(9) usage binary.
       01 idx pic 9(2) usage binary.
       linkage section.
       01 expected pic x(32).
       01 actual pic x(32). 
       01 summary.
        03 assertions-total pic 9(3) usage binary.
        03 failures-total pic 9(3) usage binary.
        03 assertions occurs 0 to 999 depending on assertions-counter.
         05 assertion-status pic x value SPACE.
          88 assertion-failed value "F".
         05 filler pic x value SPACE.
         05 assertion-suite pic x(32).
         05 filler pic x value "#".
         05 assertion-nr pic 9(2).
         05 filler pic x value SPACE.
         05 assertion-name pic x(16).
         05 filler pic x value SPACE.
         05 assertion-expected pic x(32).
         05 filler pic x(4) value " <> ".
         05 assertion-actual pic x(32).
       procedure division using expected, actual.
           set address of summary to summary-pointer.
           add 1 to assertions-total.
           add 1 to assertions-nr.
           add 1 to assertions-counter.
           move assertions-nr to assertion-nr(assertions-counter).

           move 0 to idx.
           move 0 to comparison.
           perform until idx >= length of actual
               add 1 to idx 
               compute comparison = 
                 function ord(expected(idx:1)) 
                 - function ord(actual(idx:1))
               if comparison not = 0
                   exit perform
               end-if
           end-perform.

           if comparison not = 0
               move "." to assertion-status(assertions-counter)
           else
               move "F" to assertion-status(assertions-counter)
               add 1 to failures-total
           end-if.

           move expected to assertion-expected(assertions-counter).
           move actual to assertion-actual(assertions-counter).
       end program ECBLUNEQ.
