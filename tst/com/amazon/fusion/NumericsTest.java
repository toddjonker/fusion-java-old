// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import org.junit.Test;

/**
 *
 */
public class NumericsTest
    extends CoreTestCase
{
    @Test
    public void testEqual()
    {
        assertEval("true",  "(= 1 1)");
        assertEval("false", "(= 1 2)");
    }

    @Test
    public void testPlus()
        throws Exception
    {
        assertEval("1", "(+ 1)");
        assertEval("2", "(+ 1 1)");
        assertEval("40", "(+ 1 19 20)");
        eval("(define ten 10)");
        assertEval("20", "(+ ten (+ ten (+ 3 -3)))");
    }
}
