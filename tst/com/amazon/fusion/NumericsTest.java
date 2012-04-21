// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import org.junit.Test;

public class NumericsTest
    extends CoreTestCase
{
    @Test
    public void testEqual()
        throws Exception
    {
        assertEval("true",  "(= 1 1)");
        assertEval("false", "(= 1 2)");
    }

    @Test
    public void testSum()
        throws Exception
    {
        assertEval(0, "(+)");
        assertEval(1, "(+ 1)");
        assertEval(2, "(+ 1 1)");
        assertEval(40, "(+ 1 19 20)");
        eval("(define ten 10)");
        assertEval(20, "(+ ten (+ ten (+ 3 -3)))");
    }

    @Test
    public void testSumBadType()
        throws Exception
    {
        for (String form : nonIntExpressions())
        {
            String expr = "(+ " + form + ")";
            expectArgTypeFailure(expr);

            expr = "(+ 1 " + form + " 3)";
            expectArgTypeFailure(expr);
        }
    }

    @Test
    public void testProduct()
        throws Exception
    {
        assertEval(1, "(*)");
        assertEval(4, "(* 4)");
        assertEval(20, "(* 4 5)");
        assertEval(-20, "(* 4 -5)");
        assertEval(40, "(* 4 5 2)");
        eval("(define ten 10)");
        assertEval(11, "(+ ten (+ ten (* 3 -3)))");
    }

    @Test
    public void testProductBadType()
        throws Exception
    {
        for (String form : nonIntExpressions())
        {
            String expr = "(* " + form + ")";
            expectArgTypeFailure(expr);

            expr = "(* 1 " + form + " 3)";
            expectArgTypeFailure(expr);
        }
    }

    @Test
    public void testDifference()
        throws Exception
    {
        assertEval(-1, "(- 1)");
        assertEval(-1, "(- 3 4)");
        assertEval(-6, "(- 3 4 5)");
    }

    @Test(expected = ArityFailure.class)
    public void testDifferenceNoArgs()
        throws Exception
    {
        eval("(-)");
    }

    @Test
    public void testDifferenceBadType()
        throws Exception
    {
        for (String form : nonIntExpressions())
        {
            String expr = "(- " + form + ")";
            expectArgTypeFailure(expr);

            expr = "(- 1 " + form + " 3)";
            expectArgTypeFailure(expr);
        }
    }
}
