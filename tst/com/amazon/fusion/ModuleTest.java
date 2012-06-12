// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static org.junit.Assert.assertSame;
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

    @Test(expected = UnboundIdentifierFailure.class)
    public void testUseModuleWithNoProvides()
        throws Exception
    {
        eval("(use \"tst/com/amazon/fusion/NoProvides\")");
        eval("X");
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

    @Test
    public void testTransitiveLoad()
        throws Exception
    {
        eval("(use \"tst/com/amazon/fusion/root_module.ion\")");
        assertEval(437, "leaf_var");
        FusionValue rootFn = eval("root_fn");
        FusionValue midFn  = eval("mid_fn");
        FusionValue leafFn = eval("leaf_fn");
        assertSame(leafFn, rootFn);
        assertSame(leafFn, midFn);
    }

    @Test
    public void testRepositoryLoad()
        throws Exception
    {
        eval("(use (lib \"fusion/map\"))");
        assertEval("[]", "(map + [])");
    }

    @Test
    public void testUseSyntax()
        throws Exception
    {
        expectSyntaxFailure("(use {})");
        expectSyntaxFailure("(use ())");
        expectSyntaxFailure("(use (lib))");
        expectSyntaxFailure("(use (lib \"fusion/map\" \"map\"))");
        expectSyntaxFailure("(use (lib 'fusion/map'))");
        expectSyntaxFailure("(use (lib fusion/map))");
    }
}
