// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import org.junit.Test;

/**
 * General tests of Ion DOM manipulation.
 */
public class DomTest
    extends CoreTestCase
{
    @Test
    public void testMakeSexp()
        throws Exception
    {
        assertEval("()",        "(make_sexp)");
        assertEval("(1)",       "(make_sexp 1)");
        assertEval("(1 [2] 3)", "(make_sexp 1 [2] 3)");
    }

    @Test
    public void testTypeChecks()
        throws Exception
    {
        assertEval(true,  "(is_sexp (quote ()))");
        assertEval(false, "(is_string (quote ()))");
        assertEval(false, "(is_int (void))");

        expectArityFailure("(is_bool)");
    }
}
