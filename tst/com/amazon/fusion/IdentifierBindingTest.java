// Copyright (c) 2017-2017 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionIo.isEof;
import static com.amazon.fusion.FusionSymbol.unsafeSymbolToJavaString;
import static com.amazon.fusion.FusionSyntax.isIdentifier;
import static com.amazon.fusion.FusionSyntax.unsafeIdentifierBinding;
import static com.amazon.fusion.FusionSyntax.unsafeSyntaxUnwrap;
import static com.amazon.fusion.FusionVoid.voidValue;
import static org.junit.Assert.*;
import org.junit.Test;


/**
 * Tests the unsafeIdentifierBinding Function. It's mainly being used to to
 * travel to the binding site of a reference.
 */
public class IdentifierBindingTest
    extends CoreTestCase
{
    private static final class BasicTraversalProc
        extends Procedure1
    {
        Object topLevelReference;
        Object moduleReference;
        Object renamedModuleReference;

        @Override
        Object doApply(Evaluator eval, Object arg)
            throws FusionException
        {
            if (isEof(eval, arg))
            {
                return voidValue(eval);
            }

            if (isIdentifier(eval, arg))
            {
                Object sym = unsafeSyntaxUnwrap(eval, arg);
                String name = unsafeSymbolToJavaString(eval, sym);
                switch (name)
                {
                    case "topBinding":
                    {
                        topLevelReference = arg;
                        break;
                    }
                    case "bar":
                    {
                        renamedModuleReference = arg;
                        break;
                    }
                    case "foo":
                    {
                        moduleReference = arg;
                    }
                }
            }

            return voidValue(eval);
        }
    }

    @Test
    public void testBasicAndRenameOutRequireBindings()
        throws Exception
    {
        Evaluator eval = evaluator();

        BasicTraversalProc traversal = new BasicTraversalProc();

        String module =
            "(module test_module \"/fusion\"\n" +
            "  (define (foo x)\n" +
            "    10)\n" +
            "  (provide foo (rename_out (foo bar))))";

        String source =
            "(require \"test_module\")\n" +
            "bar\n" + // imported from Module.
            "(define topBinding 10)\n" +
            "(let ((topBinding 5700))\n" + // a shadow
            "  topBinding)\n" +
            "topBinding\n" +
            "foo";

        eval(topLevel(), module);
        FusionEval.expandProgram(topLevel(), source,
                                 SourceName.forDisplay("TestFile"),
                                 traversal);


        BindingInformation topLevelInfo =
            unsafeIdentifierBinding(eval,
                                    traversal.topLevelReference);
        assertBindingAt(3, 9, topLevelInfo);
        assertEquals("TestFile",
                     topLevelInfo
                         .getSourceLocation()
                         .getSourceName()
                         .toString());


        // We expect the BindingInformation's SourceLocation to be null because
        // there is no local reference for the binding to go to. Thus, we'll jump
        // to the provide site using getModuleBindingInformation after confirming
        // that this is a required binding.
        BindingInformation moduleProvidedBinding =
            unsafeIdentifierBinding(eval,
                                    traversal.moduleReference);
        assertNull(moduleProvidedBinding.getSourceLocation());
        assertTrue(moduleProvidedBinding.isRequiredBinding());
        assertBindingAt(4, 12,
                        moduleProvidedBinding.getModuleBindingInformation());


        BindingInformation renameProvidedBinding =
            unsafeIdentifierBinding(eval,
                                    traversal.renamedModuleReference);
        assertNull(renameProvidedBinding.getSourceLocation());
        assertTrue(renameProvidedBinding.isRequiredBinding());
        assertBindingAt(4, 33, renameProvidedBinding.getModuleBindingInformation());


        BindingInformation moduleDefinedBinding = moduleProvidedBinding.target();
        assertBindingAt(2, 12, moduleDefinedBinding);


        BindingInformation renamedDefinedBinding = renameProvidedBinding.target();
        assertEquals(moduleDefinedBinding.getSourceLocation(),
                     renamedDefinedBinding.getSourceLocation());
    }


    @Test
    public void testOnlyInAndAllDefinedOutBindings()
        throws Exception
    {
        BasicTraversalProc traversal = new BasicTraversalProc();

        String onlyInModule =
            "(module only_in_module \"/fusion\"\n" +
            "  (define foo 75)\n" +
            "  (provide (all_defined_out)))";

        String enclosingModule =
            "(module enclosing \"/fusion\"\n" +
            "  (require \"only_in_module\")\n" +
            "  (provide foo))";

        String source =
            "(require (only_in \"enclosing\" foo))\n" +
            "foo";

        eval(topLevel(), onlyInModule);
        eval(topLevel(), enclosingModule);
        FusionEval.expandProgram(topLevel(), source, null, traversal);


        BindingInformation onlyInBinding =
            unsafeIdentifierBinding(evaluator(),
                                    traversal.moduleReference);
        assertBindingAt(1, 31, onlyInBinding);


        BindingInformation enclosingProvide =
            onlyInBinding.getModuleBindingInformation();
        assertBindingAt(3, 12, enclosingProvide);


        BindingInformation definition = onlyInBinding.target();
        assertBindingAt(2, 11, definition);
        assertNotEquals(enclosingProvide.getSourceLocation(),
                        definition.getSourceLocation());
    }


    private void assertBindingAt(int                expectedSourceLine,
                                 int                expectedSourceColumn,
                                 BindingInformation bindingInfo)
    {
        assertNotNull(bindingInfo);
        assertNotNull(bindingInfo.getSourceLocation());
        assertEquals(expectedSourceLine,
                     bindingInfo.getSourceLocation().getLine());
        assertEquals(expectedSourceColumn,
                     bindingInfo.getSourceLocation().getColumn());
    }
}
