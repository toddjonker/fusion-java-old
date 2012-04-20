// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.fusion.Language.ExitException;
import org.junit.Assert;
import org.junit.Test;

public class AssertTest
    extends CoreTestCase
{
    // TODO test wrong number of args

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
        expectAssertFailure("false");
    }

    private void expectAssertSuccess(String expr)
        throws Exception
    {
        // TODO test that the message isn't evaluated
        assertEval(1, "(begin (assert " + expr + " \"barney\") 1)");
    }

    @Test
    public void testAssertSuccess()
        throws Exception
    {
        expectAssertSuccess("true");
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
}
