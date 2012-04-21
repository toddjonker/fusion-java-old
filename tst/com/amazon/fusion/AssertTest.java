// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.fusion.Language.ExitException;
import org.junit.Assert;
import org.junit.Test;

public class AssertTest
    extends CoreTestCase
{
    private void expectAssertFailure(String expr)
        throws Exception
    {
        try
        {
            eval("(assert " + expr + " \"barney\")");
            Assert.fail("Expected exception");
        }
        catch (FusionAssertionFailure e)
        {
            Assert.assertEquals("barney", e.getUserMessage());
        }
    }

    @Test
    public void testAssertFailure()
        throws Exception
    {
        for (String form : IfTest.UNTRUTHY_EXPRESSIONS)
        {
            expectAssertFailure(form);
        }
    }

    private void expectAssertSuccess(String expr)
        throws Exception
    {
        assertEval(1, "(begin (assert " + expr + " \"barney\") 1)");
    }

    @Test
    public void testAssertSuccess()
        throws Exception
    {
        for (String form : IfTest.TRUTHY_EXPRESSIONS)
        {
            expectAssertSuccess(form);
        }
    }

    @Test(expected = ExitException.class)
    public void testAssertFailureWithExitingMessage()
        throws Exception
    {
        eval("(assert false (exit))");
    }

    @Test
    public void testAssertSuccessWithExitingMessage()
        throws Exception
    {
        eval("(assert true (exit))");
    }

    @Test
    public void testAssertArity()
        throws Exception
    {
        expectSyntaxFailure("(assert)");
    }

    @Test
    public void testBadTest()
        throws Exception
    {
        expectContractFailure("(assert 3)");
    }
}
