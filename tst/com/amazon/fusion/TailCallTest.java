// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import org.junit.Test;

public class TailCallTest
    extends CoreTestCase
{
    @Test
    public void testTailCall()
        throws Exception
    {
        // This code forces tail handling of 'if', 'begin', 'letrec'
        eval("(define countup" +
             "  (lambda (i limit)" +
             "    (if (= i limit) i" +
             "      (begin" +
             "        (let ((x 1))" +
               "        (letrec ((v 5))" +
             "            (countup (+ 1 i) limit)))))))");
        assertEval(1000000, "(countup 0 1000000)");
    }
}
