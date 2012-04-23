// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import org.junit.Test;

/**
 *
 */
public class BindingTest
    extends CoreTestCase
{
    @Test(expected = UnboundIdentifierFailure.class)
    public void testNoBindingAtTop()
        throws Exception
    {
        eval("g");
    }

    @Test(expected = UnboundIdentifierFailure.class)
    public void testNoBindingInFunction()
        throws Exception
    {
        eval("((func () g))");
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
        expectSyntaxFailure("(let)");
        expectSyntaxFailure("(let ((x 1)))");
        expectSyntaxFailure("(let 12 13)");
        expectSyntaxFailure("(let null.sexp 13)");
        expectSyntaxFailure("(let (12) 13)");
        expectSyntaxFailure("(let (()) 13)");
        expectSyntaxFailure("(let ((12)) 13)");
        expectSyntaxFailure("(let ((name)) 13)");
        expectSyntaxFailure("(let ((name 1) ()) 13)");
        expectSyntaxFailure("(let ((name 1) (name2)) 13)");
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
                   "(letrec ((is_even (func (n)" +
                   "                    (or (= 0 n)" +
                   "                      (is_odd (- n 1)))))" +
                   "         (is_odd (func (n)" +
                   "                   (and (not (= 0 n))" +
                   "                     (is_even (- n 1))))))" +
                   "  (is_odd 11))");
    }

    @Test
    public void testLetrecSyntax()
        throws Exception
    {
        expectSyntaxFailure("(letrec)");
        expectSyntaxFailure("(letrec ((x 1)))");
        expectSyntaxFailure("(letrec 12 13)");
        expectSyntaxFailure("(letrec null.sexp 13)");
        expectSyntaxFailure("(letrec (12) 13)");
        expectSyntaxFailure("(letrec (()) 13)");
        expectSyntaxFailure("(letrec ((12)) 13)");
        expectSyntaxFailure("(letrec ((name)) 13)");
        expectSyntaxFailure("(letrec ((name 1) ()) 13)");
        expectSyntaxFailure("(letrec ((name 1) (name2)) 13)");
    }
}
