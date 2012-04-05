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

        assertEval("(1)", "(add (quote null.sexp) 1)");
        assertEval("(1)", "(add (quote ()) 1)");
    }

    @Test
    public void testSize()
    {
        assertEval("0", "(size null.list)");
        assertEval("0", "(size [])");
        assertEval("1", "(size [1])");
        assertEval("2", "(size [2,2])");
    }

    @Test
    public void testDeepAdd()
    {
        assertEval("{f:[2]}",
                   "(let ((s {f:[]}))" +
                   "  (begin" +
                   "    (add (. s \"f\") 2)" +
                   "    s))");

        assertEval("{f:(2)}",
                   "(let ((s {f:(quote ())}))" +
                   "  (begin" +
                   "    (add (. s \"f\") 2)" +
                   "    s))");
    }
}
