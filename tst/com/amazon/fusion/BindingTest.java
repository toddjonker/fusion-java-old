// Copyright (c) 2012-2014 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import org.junit.Test;

/**
 *
 */
public class BindingTest
    extends CoreTestCase
{
    @Test
    public void testNoBindingAtTop()
        throws Exception
    {
        expectUnboundIdentifierExn("g");
    }


    @Test
    public void testNoProcBindingAtTop()
        throws Exception
    {
        expectUnboundIdentifierExn("(g)");
    }

    @Test
    public void testNoArgBindingAtTop()
        throws Exception
    {
        expectUnboundIdentifierExn("(is_int g)");
    }

    @Test
    public void testNoBindingInProcedureBody()
        throws Exception
    {
        expectUnboundIdentifierExn("((lambda () g))");
    }


    //========================================================================
    // Let

    @Test
    public void testLet()
        throws Exception
    {
        assertEval(1,  "(let ((x 1)) x)");
        assertEval(3,  "(let ((x 1) (y 2))" +
                       "  (+ x y))");
        assertEval(11, "(let ((x 1) (y 2))" +
                       "  (let [(x 10), (y x)]" +
                       "    (+ x y)))");
        assertEval(1, "(let ((u (void))) (if (is_void u) 1 2))");
    }


    @Test
    public void testNoBindings()
        throws Exception
    {
        assertEval(2, "(let () 2)");
    }

    @Test
    public void testLetMultipleBodyForms()
        throws Exception
    {
        assertEval(2, "(let ((x 1)) x (+ x x))");
    }


    @Test
    public void testLetSyntax()
        throws Exception
    {
        expectSyntaxExn("(let)");
        expectSyntaxExn("(let ((x 1)))");
        expectSyntaxExn("(let 12 13)");
        expectSyntaxExn("(let null.sexp 13)");
        expectSyntaxExn("(let (12) 13)");
        expectSyntaxExn("(let (1 2) 13)");
        expectSyntaxExn("(let (()) 13)");
        expectSyntaxExn("(let ((n 1 2)) 13)");
        expectSyntaxExn("(let ((12)) 13)");
        expectSyntaxExn("(let ((1 2)) 13)");
        expectSyntaxExn("(let ((name)) 13)");
        expectSyntaxExn("(let ((name 1) ()) 13)");
        expectSyntaxExn("(let ((name 1) (name2)) 13)");
    }


    @Test
    public void testNamedLet()
        throws Exception
    {
        assertEval(3628800,
                   "(let fac ((n 10))" +
                   "  (if (= 0 n)" +
                   "      1" +
                   "      (* n (fac (- n 1)))))");
    }


    @Test
    public void testNamedLetSyntax()
        throws Exception
    {
        expectSyntaxExn("(let null.symbol ((n 1)) 2)");
    }


    //========================================================================
    // Letrec

    @Test
    public void testLetrec()
        throws Exception
    {
        assertEval(9, "(letrec () 9)");
        assertEval(5, "(letrec ((v 5)) v)");
        assertEval(true,
                   "(letrec ((is_even (lambda (n)" +
                   "                    (or (= 0 n)" +
                   "                      (is_odd (- n 1)))))" +
                   "         (is_odd (lambda (n)" +
                   "                   (and (not (= 0 n))" +
                   "                     (is_even (- n 1))))))" +
                   "  (is_odd 11))");
    }

    @Test
    public void testLetrecSyntax()
        throws Exception
    {
        expectSyntaxExn("(letrec)");
        expectSyntaxExn("(letrec ((x 1)))");
        expectSyntaxExn("(letrec 12 13)");
        expectSyntaxExn("(letrec null.sexp 13)");
        expectSyntaxExn("(letrec (12) 13)");
        expectSyntaxExn("(letrec (1 2) 13)");
        expectSyntaxExn("(letrec (()) 13)");
        expectSyntaxExn("(letrec ((12)) 13)");
        expectSyntaxExn("(letrec ((1 2)) 13)");
        expectSyntaxExn("(letrec ((name)) 13)");
        expectSyntaxExn("(letrec ((name 1 2)) 13)");
        expectSyntaxExn("(letrec ((name 1) ()) 13)");
        expectSyntaxExn("(letrec ((name 1) (name2)) 13)");
    }
}
