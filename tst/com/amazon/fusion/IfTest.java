// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import org.junit.Test;

public class IfTest
    extends CoreTestCase
{
    @Test
    public void testBasicIf()
    {
        assertEval(1, "(if true 1 2)");
        assertEval(2, "(if false 1 2)");
        assertEval(1, "(if (if false false true) 1 2)");
    }

    @Test
    public void testNotEvaluatingOtherBranch()
        throws Exception
    {
        eval("(define boom (func () (boom)))");

        assertEval(1, "(if true 1 (boom))");
        assertEval(2, "(if false (boom) 2)");
    }

    @Test
    public void testTruthiness()
        throws Exception
    {
        eval("(define n null)");
        assertEval(2, "(if null 1 2)");
        assertEval(2, "(if null.bool 1 2)");
        assertEval(2, "(if n 1 2)");
        assertEval(2, "(if [] 1 2)");
        assertEval(2, "(if (func (x) true) 1 2)");
    }
}
