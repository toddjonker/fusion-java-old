// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static org.junit.Assert.assertSame;
import org.junit.Before;
import org.junit.Test;

/**
 *
 */
public class ModuleTest
    extends CoreTestCase
{
    @Before
    public void requires()
    {
        useTstRepo();
    }

    @Test(expected = ModuleNotFoundFailure.class)
    public void testBadLanguageSymbolInTopLevelModule()
        throws Exception
    {
        eval("(module m 'no_such_module' (define x 1))");
    }

    @Test(expected = ModuleNotFoundFailure.class)
    public void testBadQuotedLanguageSymbolInTopLevelModule()
        throws Exception
    {
        eval("(module m (quote 'no_such_module') (define x 1))");
    }

    @Test(expected = ModuleNotFoundFailure.class)
    public void testBadLanguageStringInTopLevelModule()
        throws Exception
    {
        eval("(module m \"no_such_module\" (define x 1))");
    }

    @Test(expected = FusionException.class) // ModuleNotFoundFailure gets wrapped
    public void testBadLanguageSymbolInRepoModule()
        throws Exception
    {
        eval("(use 'module/bad_lang_symbol')");
    }

    @Test(expected = FusionException.class) // ModuleNotFoundFailure gets wrapped
    public void testBadQuotedLanguageSymbolInRepoModule()
        throws Exception
    {
        eval("(use 'module/bad_quoted_lang_symbol')");
    }

    // TODO similar tests for 'use'

    @Test(expected = UnboundIdentifierFailure.class)
    public void testUseModuleWithNoProvides()
        throws Exception
    {
        eval("(use 'NoProvides')");
        eval("X");
    }


    @Test(expected = FusionException.class)
    public void testDuplicateDefinedName()
        throws Exception
    {
        eval("(use '/module/duplicate_defined_name')");
    }

    @Test(expected = FusionException.class)
    public void testDuplicateImportedName()
        throws Exception
    {
        eval("(use '/module/duplicate_imported_name')");
    }

    @Test(expected = FusionException.class)
    public void testDefineImportedName()
        throws Exception
    {
        eval("(use '/module/define_imported_name')");
    }

    @Test(expected = FusionException.class)
    public void testImportDefinedName()
        throws Exception
    {
        eval("(use '/module/import_defined_name')");
    }


    @Test(expected = UnboundIdentifierFailure.class)
    public void testIntialModuleImportsWithNoProvides()
        throws Exception
    {
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
        eval("(use root_module)");
        assertEval(437, "leaf_var");
        Object rootFn = eval("root_fn");
        Object midFn  = eval("mid_fn");
        Object leafFn = eval("leaf_fn");
        assertSame(leafFn, rootFn);
        assertSame(leafFn, midFn);
    }

    @Test
    public void testLoadFusionBase()
        throws Exception
    {
        eval("(use '/fusion/base')");
    }

    @Test
    public void testLoadFusionBaseAsLib()
        throws Exception
    {
        eval("(use (lib \"/fusion/base\"))");
    }

    @Test
    public void testRepositoryLoad()
        throws Exception
    {
        eval("(use (lib \"/fusion/list\"))");
        assertEval("[]", "(map + [])");
    }

    @Test
    public void testUseSyntax()
        throws Exception
    {
        expectSyntaxFailure("(use {})");
        expectSyntaxFailure("(use ())");
        expectSyntaxFailure("(use (lib))");
        expectSyntaxFailure("(use (lib \"/fusion/list\" \"list\"))");
        expectSyntaxFailure("(use (lib '/fusion/list'))");
        expectSyntaxFailure("(use (lib /fusion/list))");
    }

    @Test
    public void testModuleAtTopLevel()
        throws Exception
    {
        eval("(module mod '/fusion/base'" +
             "  (define M 1054)" +
            "   (define N (lambda () (+ 1 M)))" +
             "  (provide M N))");
        expectSyntaxFailure("M");

        expectFusionException("(use mod)");

        eval("(use (quote mod))");
        assertEval(1054, "M");
    }
}
