// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import org.junit.Test;

public class MacroTest
    extends CoreTestCase
{
    @Test
    public void testDefineSyntax()
        throws Exception
    {
        eval("(define_syntax S (lambda (stx) 99))");
        assertEval(99, "(S)");

        // (D v i) => (define i (lambda () v))
        eval("(define_syntax D " +
        	 "  (lambda (stx)" +
        	 "    (make_sexp (quote define) (. stx 2)" +
        	 "      (make_sexp (quote lambda) (make_sexp) (. stx 1)))))");
        eval("(D 67 x)");
        assertEval(67, "(x)");
    }
}
