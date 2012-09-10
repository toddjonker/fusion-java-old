// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import org.junit.Test;

public class TailCallTest
    extends CoreTestCase
{
    public static final int STACK_OVERFLOW_DEPTH = 8500;

    /**
     * Test that {@link #STACK_OVERFLOW_DEPTH} is large enough to cause a
     * stack overflow, otherwise the other tests below are not actually
     * testing anything.  We want to keep that number low so tests run fast.
     */
    @Test(expected = StackOverflowError.class)
    public void testStackOverflowDepth()
        throws Exception
    {
        eval("(define countup" +
            "  (lambda (i limit)" +
            "    (if (= i limit) i" +
            "      (+ 1 (countup i (- limit 1))))))");

        // Make sure the procedure is working right.
        assertEval(10, "(countup 0 10)");

        // Force a stack overflow
        eval("(countup 0 " + STACK_OVERFLOW_DEPTH + ")");
    }


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

        assertEval(STACK_OVERFLOW_DEPTH,
                   "(countup 0 " + STACK_OVERFLOW_DEPTH + ")");
    }
}
