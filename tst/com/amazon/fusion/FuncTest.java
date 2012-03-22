// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import org.junit.Test;


public class FuncTest
    extends CoreTestCase
{
    @Test
    public void testBasicFunc()
    {
        assertSelfEval("(func p 3)");
        assertEval("3", "((func p 3) 4)");
    }
    
    @Test
    public void testDefineFunc()
    {
        assertEval("(func p 3)", "(define f (func p 3))");
        assertEval("3", "(f 4)");
    }
    
    @Test
    public void testParamShadowsTopLevel()
    {
        assertEval("19", "(define p 19)");
        assertEval("(func p p)", "(define f (func p p))");
        assertEval("4", "(f 4)");
    }
    
    @Test
    public void testBodySeesTopLevel()
    {
        assertEval("19", "(define p 19)");
        assertEval("(func p1 p)", "(define f (func p1 p))");
        assertEval("19", "(f 4)");
        assertEval("20", "(define p 20)");
        assertEval("20", "(f 4)");
    }
    
    @Test
    public void testBodySeesOuterParam()
    {
        assertEval("19", "(define p 19)");
        assertEval("(func p (func p1 p))", "(define f (func p (func p1 p)))");
        assertEval("(func p1 p)", "(f 4)");
        assertEval("4", "((f 4) 5)");
    }
    
    @Test
    public void testFunctionArguments()
    {
        assertEval("19", "(define p 19)");
        assertEval("(func g (func p (g p)))", 
                   "(define f (func g (func p (g p))))");
        assertEval("(func p (g p))", "(f 4)");
        assertEval("76", "((f (func x 76)) 5)");
        assertEval("(func p (g p))", "(define f2 (f (func x 77)))");
        assertEval("77", "(f2 5)");
    }
}
