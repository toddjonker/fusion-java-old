// Copyright (c) 2012-2014 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import org.junit.Test;

/**
 *
 */
public class BindingTest
    extends CoreTestCase
{
    @Test
    public void testNoBindingAtTop()
        throws Exception
    {
        expectUnboundIdentifierExn("g");
    }


    @Test
    public void testNoProcBindingAtTop()
        throws Exception
    {
        expectUnboundIdentifierExn("(g)");
    }

    @Test
    public void testNoArgBindingAtTop()
        throws Exception
    {
        expectUnboundIdentifierExn("(is_int g)");
    }

    @Test
    public void testNoBindingInProcedureBody()
        throws Exception
    {
        expectUnboundIdentifierExn("((lambda () g))");
    }
}
