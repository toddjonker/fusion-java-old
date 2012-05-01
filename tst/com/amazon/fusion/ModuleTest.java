// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import org.junit.Test;

/**
 *
 */
public class ModuleTest
    extends CoreTestCase
{
    @Test
    public void testModuleForm()
        throws Exception
    {
        eval("(define M (module (define X 735)))");
        eval("(use M)");
        assertEval(735, "X");
    }

    @Test
    public void testModuleFromFile()
        throws Exception
    {
        eval("(use \"tst/com/amazon/fusion/Module1\")");
        assertEval(214, "X");
    }
}
