// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import org.junit.Assert;
import org.junit.Ignore;
import org.junit.Test;

public class IfTest
    extends CoreTestCase
{
    public static final String[] TRUTHY_EXPRESSIONS =
    {
        "true",
        "((func () true))",
    };

    public static final String[] UNTRUTHY_EXPRESSIONS =
    {
        "false",
        "((func () false))",
    };

    public static final String[] FAILING_FORMS =
    {
         "null.bool",
         "undef",
    };


    @Test
    public void testBasicIf()
        throws Exception
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

    @Test @Ignore
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

    @Test
    public void testTruthyIf()
        throws Exception
    {
        for (String form : TRUTHY_EXPRESSIONS)
        {
            assertEval(1, "(if " + form + " 1 2)");
        }
    }

    @Test
    public void testUntruthyIf()
        throws Exception
    {
        for (String form : UNTRUTHY_EXPRESSIONS)
        {
            assertEval(2, "(if " + form + " 1 2)");
        }
    }

    @Test
    public void testFailingIf()
    {
        for (String form : FAILING_FORMS)
        {
            try
            {
                String ifExpr = "(if " + form + " 1 2)";
                eval(ifExpr);
                Assert.fail("Expected exception from " + ifExpr);
            }
            catch (FusionException e) { }
        }
    }
}
