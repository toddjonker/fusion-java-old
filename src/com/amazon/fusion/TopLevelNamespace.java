// Copyright (c) 2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.fusion.ModuleNamespace.ModuleBinding;
import java.util.Iterator;
import java.util.Set;


/**
 * Extended prepare-time {@link Namespace} that knows it's a top-level.
 * <p>
 * The tricky part here is getting the precedence correct between imports and
 * top-level definitions.  The rule is that the last occurrence wins.  To
 * implement this we keep a counter of the number of times that
 * {@link #use(ModuleInstance)} has been called, record that on bindings when
 * they are defined or redefined, and compare those numbers to determine which
 * binding has precedence.
 */
class TopLevelNamespace
    extends Namespace
{
    private static final class TopLevelBinding
        extends NsBinding
    {
        private int myPrecedence;

        private TopLevelBinding(SyntaxSymbol identifier, int address,
                                int precedence)
        {
            super(identifier, address);
            myPrecedence = precedence;
        }

        @Override
        public CompiledForm compileTopReference(Evaluator eval,
                                                Environment env,
                                                SyntaxSymbol id)
            throws FusionException
        {
            return new CompiledTopVariableReference(myAddress);
        }

        @Override
        public String toString()
        {
            return "{{{TopLevelBinding " + getIdentifier() + "}}}";
        }
    }


    private static final class TopLevelRequireWrap
        extends ModuleRenameWrap
    {
        private final int myPrecedence;

        TopLevelRequireWrap(ModuleInstance module, int precedence)
        {
            super(module);
            myPrecedence = precedence;
        }

        @Override
        Binding resolve(String name,
                        Iterator<SyntaxWrap> moreWraps,
                        Set<Integer> returnMarks)
        {
            Binding local = localResolveMaybe(name);
            assert local == null || local instanceof ModuleBinding;

            if (moreWraps.hasNext())
            {
                SyntaxWrap nextWrap = moreWraps.next();
                Binding earlier =
                    nextWrap.resolve(name, moreWraps, returnMarks);
                assert earlier == null
                    || earlier instanceof FreeBinding
                    || earlier instanceof NsBinding;

                if (local == null) return earlier;

                // The top-level "define" bindings are always earlier than
                // imported bindings, since the namespace wrap is added first.
                if (earlier instanceof TopLevelBinding)
                {
                    int definePrecedence =
                        ((TopLevelBinding) earlier).myPrecedence;
                    return (myPrecedence < definePrecedence ? earlier : local);
                }
            }

            return local;
        }
    }



    private int myCurrentPrecedence = 0;

    /**
     * Constructs a top-level namespace. Any bindings will need to be
     * {@code require}d or {@code define}d.
     */
    TopLevelNamespace(ModuleRegistry registry)
    {
        super(registry);
    }


    @Override
    ModuleIdentity getModuleId()
    {
        return null;
    }


    @Override
    NsBinding newBinding(SyntaxSymbol identifier, int address)
    {
        return new TopLevelBinding(identifier, address, myCurrentPrecedence);
    }


    @Override
    SyntaxSymbol predefine(SyntaxSymbol identifier, SyntaxValue formForErrors)
        throws FusionException
    {
        identifier = identifier.makeFree();

        NsBinding binding = localResolve(identifier);
        if (binding == null)
        {
            binding = addBinding(identifier);
        }
        else
        {
            // Give this pre-existing binding precedence over any imports that
            // may have happened since the last time it was defined.
            ((TopLevelBinding) binding).myPrecedence = myCurrentPrecedence;
        }

        return identifier.copyReplacingBinding(binding);
    }


    @Override
    void use(ModuleInstance module)
        throws FusionException
    {
        addWrap(new TopLevelRequireWrap(module, myCurrentPrecedence));
        myCurrentPrecedence++;
    }
}
