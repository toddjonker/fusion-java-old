// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import org.junit.Test;

/**
 *
 */
public class ListTest
    extends CoreTestCase
{
    @Test
    public void testAdd()
    {
        assertEval("[1]", "(add null.list 1)");
        assertEval("[1]", "(add [] 1)");
    }

    @Test
    public void testSize()
    {
        assertEval("0", "(size null.list)");
        assertEval("0", "(size [])");
        assertEval("1", "(size [1])");
        assertEval("2", "(size [2,2])");
    }
}
