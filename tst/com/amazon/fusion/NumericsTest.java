// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import java.math.BigInteger;
import org.junit.Test;

public class NumericsTest
    extends CoreTestCase
{

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
    public void testSumArgType()
        throws Exception
    {
        for (String form : nonIntExpressions())
        {
            String expr = "(+ " + form + ")";
            expectArgTypeFailure(expr, 0);

            expr = "(+ 1 " + form + " 3)";
            expectArgTypeFailure(expr, 1);
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
    public void testProductArgType()
        throws Exception
    {
        for (String form : nonIntExpressions())
        {
            String expr = "(* " + form + ")";
            expectArgTypeFailure(expr, 0);

            expr = "(* 1 " + form + " 3)";
            expectArgTypeFailure(expr, 1);
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
    public void testDifferenceArgType()
        throws Exception
    {
        for (String form : nonIntExpressions())
        {
            String expr = "(- " + form + ")";
            expectArgTypeFailure(expr, 0);

            expr = "(- 1 " + form + " 3)";
            expectArgTypeFailure(expr, 1);
        }
    }

    @Test
    public void testAtoi()
        throws Exception
    {
        assertBigInt(5, "(to_int \"5\")");
        assertBigInt(-2, "(to_int \"-2\")");

        String bigIntStr = "8888888888888888888888888888888888888888888888888888888888";
        BigInteger expected = new BigInteger(bigIntStr);
        assertEval(expected,"(to_int \""+bigIntStr+"\")");
    }

    @Test
    public void testAtoiFail()
        throws Exception
    {
        expectFusionException("(to_int \"hello\")");
        expectArityFailure("(to_int)");

        expectArgTypeFailure("(to_int 2)",0);
        expectArgTypeFailure("(to_int null.string)", 0);
        expectArgTypeFailure("(to_int \"2.33332\")", 0);
    }

    @Test
    public void testItoa()
       throws Exception
    {
        assertString("5","(to_string 5)");
        assertString("-2","(to_string -2)");
        assertString("hello","(to_string \"hello\")");

        String reallyBigNumber = "8888888888888888888888888888888888888888888888888888888888";
        assertEval("\""+reallyBigNumber+"\"", "(to_string "+reallyBigNumber+")");
    }

    @Test
    public void testItoaFail()
       throws Exception
    {
        expectArgTypeFailure("(to_string 2.33332)", 0);
        expectArgTypeFailure("(to_string null.int)", 0);

        expectArityFailure("(to_string)");
        expectArityFailure("(to_string 2 2)");
    }
}
