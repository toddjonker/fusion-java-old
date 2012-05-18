// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import org.junit.Test;

/**
 *
 */
public class ForTest
    extends CoreTestCase
{
    @Test
    public void testBasicForList()
        throws Exception
    {
        assertEval("[]",   "(for_list () 73)");
        assertEval("[]",   "(for_list ((e [])) 73)");
        assertEval("[73]", "(for_list ((e [1])) 73)");
        assertEval("[1]",  "(for_list ((e [1])) e)");

        assertEval("[2, 4, 6]",
                   "(for_list ((e [1, 2, 3])" +
                   "           (f [1, 2, 3]))" +
                   "  (+ e f))");
        assertEval("[2, 4]",
                   "(for_list ((e [1, 2, 3])" +
                   "           (f [1, 2]))" +
                   "  (+ e f))");
        assertEval("[2, 4]",
                   "(for_list ((e (quote (1 2)))" +
                   "           (f [1, 2, 3]))" +
                   "  (+ e f))");
    }
}
