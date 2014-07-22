// Copyright (c) 2014 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionIo.isEof;
import static com.amazon.fusion.FusionSexp.isSexp;
import static com.amazon.fusion.FusionSexp.unsafePairHead;
import static com.amazon.fusion.FusionSexp.unsafeSexpSize;
import static com.amazon.fusion.FusionSymbol.unsafeSymbolToJavaString;
import static com.amazon.fusion.FusionSyntax.isIdentifier;
import static com.amazon.fusion.FusionSyntax.isSyntax;
import static com.amazon.fusion.FusionSyntax.unsafeFreeIdentifierEqual;
import static com.amazon.fusion.FusionSyntax.unsafeSyntaxUnwrap;
import static com.amazon.fusion.FusionVoid.voidValue;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import org.junit.Test;


public class ExpandProgramTest
    extends CoreTestCase
{
    private static final class CoreFormCollector
        extends Procedure1
    {
        Object lambdaId;
        Object moduleId;

        @Override
        Object doApply(Evaluator eval, Object arg)
            throws FusionException
        {
            if (! isEof(eval, arg))
            {
                assertTrue(isSyntax(eval, arg));
                Object sexp = unsafeSyntaxUnwrap(eval, arg);
                assertTrue(isSexp(eval, sexp));
                assertTrue(unsafeSexpSize(eval, sexp) != 0);

                Object first = unsafePairHead(eval, sexp);
                assertTrue(isIdentifier(eval, first));
                Object sym  = unsafeSyntaxUnwrap(eval, first);
                String name = unsafeSymbolToJavaString(eval, sym);

                // In general one can't assume that an identifer named "lambda"
                // refers to the kernel's "lambda". But in this test case we
                // are processing source code that already has those core
                // forms. Actually this is still somewhat brittle since the
                // lambda exported from /fusion could be a macro, but it's okay
                // for our purposes here.

                switch (name)
                {
                    case "lambda":
                    {
                        lambdaId = first;
                        break;
                    }
                    case "module":
                    {
                        moduleId = first;
                        break;
                    }
                }
            }

            return voidValue(eval);
        }
    }

    @Test
    public void testFindingCoreForms()
        throws Exception
    {
        CoreFormCollector collector = new CoreFormCollector();

        String source =
            "(lambda () 1) " +
            "(module M '/fusion' 1)";

        FusionEval.expandProgram(this.topLevel(), source, null, collector);

        assertNotNull(collector.moduleId);

        GlobalState globals = evaluator().getGlobalState();
        assertTrue(unsafeFreeIdentifierEqual(evaluator(),
                                             collector.lambdaId,
                                             globals.myKernelLambdaIdentifier));
        assertTrue(unsafeFreeIdentifierEqual(evaluator(),
                                             collector.moduleId,
                                             globals.myKernelModuleIdentifier));
    }
}
