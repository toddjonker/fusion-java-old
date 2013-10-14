// Copyright (c) 2012-2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
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
        throws FusionException
    {
        useTstRepo();
    }

    @Test(expected = ModuleNotFoundException.class)
    public void testBadLanguageSymbolInTopLevelModule()
        throws Exception
    {
        eval("(module m '/no_such_module' (define x 1))");
    }

    @Test(expected = SyntaxException.class)
    public void testBadQuotedLanguageSymbolInTopLevelModule()
        throws Exception
    {
        eval("(module m (quote 'no_such_module') (define x 1))");
    }

    @Test(expected = ModuleNotFoundException.class)
    public void testBadLanguageStringInTopLevelModule()
        throws Exception
    {
        eval("(module m \"no_such_module\" (define x 1))");
    }

    @Test(expected = ModuleNotFoundException.class)
    public void testRelativeLanguageStringInTopLevelModule()
        throws Exception
    {
        // TODO FUSION-151 this isn't well-defined yet and should be rejected.
        eval("(module m \"fusion\" (define x 1))");
    }

    @Test(expected = ModuleNotFoundException.class)
    public void testTopLevelLanguageInTopLevelModule()
        throws Exception
    {
        eval("(module lang '/fusion/base')");

        // TODO FUSION-151 this isn't well-defined yet and should be rejected.
        eval("(module m \"lang\" (define x 1))");
    }

    // TODO similar tests for 'require' to the language tests above


    @Test(expected = FusionException.class) // ModuleNotFoundException gets wrapped
    public void testBadLanguageSymbolInRepoModule()
        throws Exception
    {
        eval("(require '/module/bad_lang_symbol')");
    }

    @Test(expected = FusionException.class) // ModuleNotFoundException gets wrapped
    public void testBadLanguageSymbolInRepoModule2()
        throws Exception
    {
        eval("(require \"/module/bad_lang_symbol\")");
    }

    @Test(expected = FusionException.class) // ModuleNotFoundException gets wrapped
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

    @Test(expected = FusionException.class)
    public void testRequireNonModule()
        throws Exception
    {
        eval("(require \"/nonmodule/trivialDefine\")");
    }

    @Test
    public void testRequireUnderTop()
        throws Exception
    {
        String code =
            "(module m '/fusion'" +
            "  (if true (require '/fusion/list') false))";
        expectSyntaxFailure(code);
    }

    /** Traps an infinite-loop bug in partial expansion. */
    @Test(expected = FusionException.class)
    public void testBadSexpAtModuleLevel()
        throws Exception
    {
        eval("(module m '/fusion' (1))");
    }

    @Test
    public void testModuleAtTopLevel()
        throws Exception
    {
        eval("(module mod '/fusion'" +
             "  (define M 1054)" +
            "   (define N (lambda () (+ 1 M)))" +
             "  (provide M N))");
        expectSyntaxFailure("M");

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
        eval("(module x '/fusion'" +
             "  (require '/fusion/experimental/syntax')" +
             "  (define_syntax broken (lambda (s) x))" +
             "  (provide broken))");
    }


    @Test
    public void testModuleCircularity()
        throws Exception
    {
        try
        {
            topLevel().requireModule("/cycle0");
            fail("Expected exception");
        }
        catch (FusionException e)
        {
            String message = e.getMessage();
            assertTrue(message.contains("/cycle1"));
            assertTrue(message.contains("/cycle2"));
            assertTrue(message.contains("/cycle3"));
        }
    }


    @Test
    public void testTopLevelLocalModuleIsolation()
        throws Exception
    {
        TopLevel top1 = topLevel();
        TopLevel top2 = runtime().makeTopLevel();

        top1.eval("(module M '/fusion' (define v 1) (provide v))");
        top2.eval("(module M '/fusion' (define v 2) (provide v))");

        top1.eval("(require M)");
        top2.eval("(require M)");

        assertEval(top1, 1, "v");
        assertEval(top2, 2, "v");
    }
}
