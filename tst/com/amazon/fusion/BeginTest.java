// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import org.junit.Test;

public class BeginTest
    extends CoreTestCase
{
    @Test
    public void testBegin()
    {
        assertEval(1, "(begin 1)");
        assertEval(2, "(begin 1 2)");
        assertEval("[]", "(begin 1 2 [])");
    }
}
