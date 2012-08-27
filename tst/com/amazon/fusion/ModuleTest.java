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
    @Test(expected = UnboundIdentifierFailure.class)
    public void testUseModuleWithNoProvides()
        throws Exception
    {
        useTstRepo();
        eval("(use 'NoProvides')");
        eval("X");
    }


    @Test(expected = UnboundIdentifierFailure.class)
    public void testIntialModuleImportsWithNoProvides()
        throws Exception
    {
        useTstRepo();
        eval("(module M 'NoProvides' X)");
    }


    @Test(expected = FusionException.class)
    public void testUseNonSexp()
        throws Exception
    {
        eval("(use \"tst-data/hello.ion\")");
    }

    @Test(expected = FusionException.class)
    public void testUseNonModule()
        throws Exception
    {
        eval("(use \"tst-data/trivialDefine.ion\")");
    }

    @Test
    public void testTransitiveLoad()
        throws Exception
    {
        useTstRepo();
        eval("(use root_module)");
        assertEval(437, "leaf_var");
        FusionValue rootFn = eval("root_fn");
        FusionValue midFn  = eval("mid_fn");
        FusionValue leafFn = eval("leaf_fn");
        assertSame(leafFn, rootFn);
        assertSame(leafFn, midFn);
    }

    @Test
    public void testLoadFusionBase()
        throws Exception
    {
        eval("(use 'fusion/base')");
    }

    @Test
    public void testLoadFusionBaseAsLib()
        throws Exception
    {
        eval("(use (lib \"fusion/base\"))");
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
