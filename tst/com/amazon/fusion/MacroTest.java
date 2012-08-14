// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import org.junit.Before;
import org.junit.Test;

public class MacroTest
    extends CoreTestCase
{
    @Override @Before
    public void setUp()
        throws Exception
    {
        super.setUp();
        eval("(use 'fusion/syntax')");
    }


    @Test
    public void testDefineSyntax()
        throws Exception
    {
        eval("(define_syntax S (lambda (stx) (quasisyntax 99)))");
        assertEval(99, "(S)");

        // (D v i) => (define i (lambda () v))
        eval("(define_syntax D " +
             "  (lambda (stx)" +
             "    (quasisyntax" +
             "      (define (unsyntax (syntax_get stx 2))" +
             "        (lambda () (unsyntax (syntax_get stx 1)))))))");
        eval("(D 67 x)");
        assertEval(67, "(x)");
    }
}
