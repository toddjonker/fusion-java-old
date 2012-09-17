// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import org.junit.Test;


public class DefineTest
    extends CoreTestCase
{
    @Test
    public void testRedefine()
        throws Exception
    {
        eval("(define x 93)");
        assertEval(93, "x");
        eval("(define y x)");
        assertEval(93, "x");
        assertEval(93, "y");
        eval("(define x 99)");
        assertEval(99, "x");
        assertEval(93, "y");
    }

    @Test
    public void badDefineSyntax()
        throws Exception
    {
        expectSyntaxFailure("(define)");
        expectSyntaxFailure("(define x)");
        expectSyntaxFailure("(define x 2 3)");
        expectSyntaxFailure("(define 1 2)");
        expectSyntaxFailure("(define null.symbol 2)");
    }
}
