// Copyright (c) 2012-2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

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
        eval("(module m '/no_such_module' (define x 1))");
    }

    @Test(expected = SyntaxFailure.class)
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

    @Test(expected = ModuleNotFoundFailure.class)
    public void testRelativeLanguageStringInTopLevelModule()
        throws Exception
    {
        // TODO FUSION-151 this isn't well-defined yet and should be rejected.
        eval("(module m \"fusion\" (define x 1))");
    }

    @Test(expected = ModuleNotFoundFailure.class)
    public void testTopLevelLanguageInTopLevelModule()
        throws Exception
    {
        eval("(module lang '/fusion/base')");

        // TODO FUSION-151 this isn't well-defined yet and should be rejected.
        eval("(module m \"lang\" (define x 1))");
    }

    // TODO similar tests for 'use' to the language tests above


    @Test(expected = FusionException.class) // ModuleNotFoundFailure gets wrapped
    public void testBadLanguageSymbolInRepoModule()
        throws Exception
    {
        eval("(require '/module/bad_lang_symbol')");
    }

    @Test(expected = FusionException.class) // ModuleNotFoundFailure gets wrapped
    public void testBadLanguageSymbolInRepoModule2()
        throws Exception
    {
        eval("(require \"/module/bad_lang_symbol\")");
    }

    @Test(expected = FusionException.class) // ModuleNotFoundFailure gets wrapped
    public void testBadQuotedLanguageSymbolInRepoModule()
        throws Exception
    {
        eval("(require '/module/bad_quoted_lang_symbol')");
    }


    @Test(expected = UnboundIdentifierFailure.class)
    public void testUseModuleWithNoProvides()
        throws Exception
    {
        eval("(require '/NoProvides')");
        eval("X");
    }


    @Test(expected = FusionException.class)
    public void testDuplicateDefinedName()
        throws Exception
    {
        eval("(require '/module/duplicate_defined_name')");
    }

    @Test(expected = FusionException.class)
    public void testDuplicateImportedName()
        throws Exception
    {
        eval("(require \"/module/duplicate_imported_name\")");
    }

    @Test(expected = FusionException.class)
    public void testDefineImportedName()
        throws Exception
    {
        eval("(require '/module/define_imported_name')");
    }

    @Test(expected = FusionException.class)
    public void testImportDefinedName()
        throws Exception
    {
        eval("(require '/module/import_defined_name')");
    }


    @Test(expected = UnboundIdentifierFailure.class)
    public void testIntialModuleImportsWithNoProvides()
        throws Exception
    {
        eval("(module M '/NoProvides' X)");
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

    @Test @Deprecated  // TODO FUSION-133 remove 'use'
    public void testLoadFusionBase()
        throws Exception
    {
        eval("(use '/fusion/base')");
    }

    @Test @Deprecated  // TODO FUSION-133 remove 'use'
    public void testLoadFusionBaseAsLib()
        throws Exception
    {
        eval("(use (lib \"/fusion/base\"))");
    }

    @Test @Deprecated  // TODO FUSION-133 remove 'use'
    public void testRepositoryLoad()
        throws Exception
    {
        eval("(use (lib \"/fusion/list\"))");
        assertEval("[]", "(map + [])");
    }

    @Test @Deprecated  // TODO FUSION-133 remove 'use'
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
    public void testBadRequireSyntax()
        throws Exception
    {
        expectSyntaxFailure("(require)");
        expectSyntaxFailure("(require {})");
        expectSyntaxFailure("(require ())");
        expectSyntaxFailure("(require (lib))");
        expectSyntaxFailure("(require (lib \"/fusion/list\"))");
    }

    @Test
    public void testRequireUnderTop()
        throws Exception
    {
        String code =
            "(module m '/fusion/base'" +
            "  (if true (require '/fusion/list') false))";
        expectSyntaxFailure(code);
    }

    /** Traps an infinite-loop bug in partial expansion. */
    @Test(expected = FusionException.class)
    public void testBadSexpAtModuleLevel()
        throws Exception
    {
        eval("(module m '/fusion/base' (1))");
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

        expectFusionException("(use mod)"); // TODO FUSION-133 remove 'use'

        eval("(require mod)");
        assertEval(1054, "M");
    }

    /**
     * We shouldn't fail `provide` at expand-time, since that may hide the
     * compile-time errors that caused provide to fail.
     */
    @Test(expected=UnboundIdentifierFailure.class)
    public void testProvideFailsLate()
        throws Exception
    {
        eval("(module x '/fusion/base'" +
             "  (require '/fusion/syntax')" +
             "  (define_syntax broken (lambda (s) x))" +
             "  (provide broken))");
    }
}
