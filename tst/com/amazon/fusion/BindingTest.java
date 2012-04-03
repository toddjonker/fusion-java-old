// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import org.junit.Test;

/**
 *
 */
public class BindingTest
    extends CoreTestCase
{
    @Test
    public void testLet()
    {
        assertEval("1",  "(let ((x 1)) x)");
        assertEval("3",  "(let ((x 1) (y 2))" +
        		         "  (+ x y))");
        assertEval("11", "(let ((x 1) (y 2))" +
        		         "  (let [(x 10), (y x)]" +
        		         "    (+ x y)))");
    }
}
