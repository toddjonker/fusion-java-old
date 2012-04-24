// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import org.junit.Test;


public class FuncTest
    extends CoreTestCase
{
    @Test
    public void testBasicFunc()
        throws Exception
    {
        assertEval(3, "((func (p) 3) 4)");
    }

    @Test
    public void testDefineFunc()
        throws Exception
    {
        eval("(define f (func (p) 3))");
        assertEval(3, "(f 4)");
    }

    @Test
    public void testParamShadowsTopLevel()
        throws Exception
    {
        eval("(define p 19)");
        eval("(define f (func (p) p))");
        assertEval(4, "(f 4)");
    }

    @Test
    public void testBodySeesTopLevel()
        throws Exception
    {
        eval("(define p 19)");
        eval("(define f (func (p1) p))");
        assertEval(19, "(f 4)");
        eval("(define p 20)");
        assertEval(20, "(f 4)");
    }

    @Test
    public void testBodySeesOuterParam()
        throws Exception
    {
        eval("(define p 19)");
        eval("(define f (func (p) (func (p1) p)))");
        evalToFunction("(f 4)");
        assertEval(4, "((f 4) 5)");
    }

    @Test
    public void testFunctionArguments()
        throws Exception
    {
        eval("(define p 19)");
        eval("(define f (func (g) (func (p) (g p))))");
        evalToFunction("(f 4)");
        assertEval(76, "((f (func (x) 76)) 5)");
        eval("(define f2 (f (func (x) 77)))");
        assertEval(77, "(f2 5)");
    }

    @Test
    public void testNoParams()
        throws Exception
    {
        assertEval("true", "((func () true))");
        assertEval(13, "((func (f) (f)) (func () 13))");
        assertEval(1, "((let ((x 1)) (func () x)))");
    }

    @Test
    public void testMultipleArguments()
        throws Exception
    {
        assertEval(1, "((func (x y) x) 1 2)");
        assertEval(2, "((func (x y) y) 1 2)");
        assertEval(2, "((func (x y) (x y)) (func (x) x) 2)");

        eval("(define i (func ( x )x))");
        assertEval(2, "((func (x y) ((y y) (y x))) 2 i)");
    }

    @Test
    public void testMultipleBodyForms()
        throws Exception
    {
        assertEval(2, "((func () 1 2))");
        assertEval(1, "((func (x y) y x) 1 2)");
    }

    @Test
    public void testArgSyntaxFailure()
        throws Exception
    {
        expectSyntaxFailure("((func (x) 1) (if 2))");
    }

    @Test(expected = ArityFailure.class)
    public void testWrongNumberOfArguments()
        throws Exception
    {
        assertEval(1, "((func (x y) (x y)) (func () 1) 2)");
    }

    @Test
    public void testTailCall()
        throws Exception
    {
        // This code forces tail handling of 'if', 'begin', 'letrec'
        eval("(define countup" +
             "  (func (i limit)" +
             "    (if (= i limit) i" +
             "      (begin" +
             "        (let ((x 1))" +
               "        (letrec ((v 5))" +
             "            (countup (+ 1 i) limit)))))))");
        assertEval(1000000, "(countup 0 1000000)");
    }
}
