// Copyright (c) 2012-2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

public class AssertTest
    extends CoreTestCase
{
    @Before
    public void requires()
        throws Exception
    {
        topLevel().requireModule("/fusion/exception");
    }


    private void expectAssertFailure(String expr)
        throws Exception
    {
        try
        {
            eval("(assert " + expr + ")");
            Assert.fail("Expected exception");
        }
        catch (FusionAssertionFailure e)
        {
            Assert.assertEquals(null, e.getUserMessage());
        }

        try
        {
            eval("(assert " + expr + " \"barney\")");
            Assert.fail("Expected exception");
        }
        catch (FusionAssertionFailure e)
        {
            Assert.assertEquals("barney", e.getUserMessage());
        }

        try
        {
            eval("(assert " + expr + " \"barney\" 13)");
            Assert.fail("Expected exception");
        }
        catch (FusionAssertionFailure e)
        {
            Assert.assertEquals("barney13", e.getUserMessage());
        }
    }

    @Test
    public void testAssertFailure()
        throws Exception
    {
        for (String form : BooleanTest.UNTRUTHY_EXPRESSIONS)
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
        for (String form : BooleanTest.TRUTHY_EXPRESSIONS)
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
}
