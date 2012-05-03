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
    public void testUseModuleFromFile()
        throws Exception
    {
        eval("(use \"tst/com/amazon/fusion/Module1\")");
        assertEval(214, "X");
    }

    @Test(expected = FusionException.class)
    public void testUseNonSexp()
        throws Exception
    {
        eval("(use \"tst/com/amazon/fusion/hello.ion\")");
    }

    @Test(expected = FusionException.class)
    public void testUseNonModule()
        throws Exception
    {
        eval("(use \"tst/com/amazon/fusion/trivialDefine.ion\")");
    }
}
