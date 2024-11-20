// Copyright (c) 2012-2014 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import org.junit.Before;
import org.junit.Test;
import org.junit.jupiter.api.Assertions;

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
            Assertions.fail("Expected exception");
        }
        catch (FusionAssertionException e)
        {
            Assertions.assertEquals(null, e.getUserMessage());
        }

        try
        {
            eval("(assert " + expr + " \"barney\")");
            Assertions.fail("Expected exception");
        }
        catch (FusionAssertionException e)
        {
            Assertions.assertEquals("barney", e.getUserMessage());
        }

        try
        {
            eval("(assert " + expr + " \"barney\" 13)");
            Assertions.fail("Expected exception");
        }
        catch (FusionAssertionException e)
        {
            Assertions.assertEquals("barney13", e.getUserMessage());
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
        expectSyntaxExn("(assert)");
    }
}
