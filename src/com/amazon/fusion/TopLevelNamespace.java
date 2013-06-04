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
 * {@link #require(ModuleInstance)} has been called, record that on bindings when
 * they are defined or redefined, and compare those numbers to determine which
 * binding has precedence.
 */
class TopLevelNamespace
    extends Namespace
{
    /**
     * Maps from top-level names to definitions and/or module bindings.
     */
    static final class TopLevelBinding
        extends NsBinding
    {
        /**
         * Reference to the active binding for my identifier, either this
         * instance (when the top-level definition is active) or a binding
         * from an import.
         */
        private NsBinding myTarget;

        /**
         * The precedence of the top-level definition (stored at my address).
         */
        private int myPrecedence;

        private TopLevelBinding(SyntaxSymbol identifier, int address,
                                int precedence)
        {
            super(identifier, address);
            myTarget = this;
            myPrecedence = precedence;
        }

        private TopLevelBinding(TopLevelRequireBinding required)
        {
            super(required.myBinding.getIdentifier(), -1);
            myTarget = required.myBinding;
            myPrecedence = -1;
        }

        @Override
        public Binding originalBinding()
        {
            return myTarget;
        }

        @Override
        public boolean sameTarget(Binding other)
        {
            return myTarget == other.originalBinding();  // XXX == ???
        }

        @Override
        public Object lookup(Environment env)
        {
            if (myTarget == this)
            {
                return super.lookup(env);
            }
            return myTarget.lookup(env);
        }


        @Override
        public CompiledForm compileReference(Evaluator eval, Environment env)
            throws FusionException
        {
            if (myTarget == this)
            {
                return super.compileReference(eval, env);
            }
            return myTarget.compileReference(eval, env);
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
        CompiledForm compileDefine(Evaluator eval,
                                   Environment env,
                                   CompiledForm valueForm)
        {
            assert myTarget == this;
            return super.compileDefine(eval, env, valueForm);
        }

        @Override
        CompiledForm compileDefineSyntax(Evaluator eval,
                                         Environment env,
                                         CompiledForm valueForm)
        {
            assert myTarget == this;
            return super.compileDefineSyntax(eval, env, valueForm);
        }

        @Override
        public String toString()
        {
            return "{{{TopLevelBinding " +
                (myTarget == this ? getIdentifier().debugString() : myTarget) +
                "}}}";
        }
    }


    private static final class TopLevelWrap
        extends EnvironmentRenameWrap
    {
        private final TopLevelNamespace myTopNs;

        TopLevelWrap(TopLevelNamespace ns)
        {
            super(ns);
            myTopNs = ns;
        }


        @Override
        Binding resolveTop(String name,
                           Iterator<SyntaxWrap> moreWraps,
                           Set<Integer> returnMarks)
        {
            if (moreWraps.hasNext())
            {
                SyntaxWrap nextWrap = moreWraps.next();
                return nextWrap.resolve(name, moreWraps, returnMarks);
            }
            return null;
        }

        @Override
        TopLevelBinding resolve(String name,
                                Iterator<SyntaxWrap> moreWraps,
                                Set<Integer> returnMarks)
        {
            // Check our environment directly. This will handle identifiers
            // that have top-level definitions, but not those that only map to
            // module bindings.

            // TODO FUSION-117 If the source has had namespace-syntax-introduce
            // then this binding may be from an earlier wrap, and may be from
            // a different top-level!

            TopLevelBinding definedBinding = (TopLevelBinding)
                super.resolve(name, moreWraps, returnMarks);

            assert (definedBinding == null
                    || definedBinding.myAddress == -1
                    || myTopNs.ownsBinding(definedBinding));

            // Look for an imported binding, then decide which one wins.

            // TODO Perhaps all this should happen at import-time, so we only
            // have to search our environment.  That saves repeated work when
            // resolving identifiers, at the cost of forcing TopLevelBindings
            // to be created for every imported binding, and at the cost of
            // having a much larger namespace lookup table.

            // TODO FUSION-63 The resolve() is broken if we have a module
            // binding with a name that has a foreign lexical context, since
            // this only looks by name.

            TopLevelRequireBinding requiredBinding = (TopLevelRequireBinding)
                myTopNs.myRequiredModuleWraps.resolve(name);
            if (requiredBinding == null)
            {
                // No matching import, so use any top-level definition.
                return definedBinding;
            }
            if (definedBinding == null)
            {
                // We have an import, but no top-level definition.
                // TODO this is too common to throw away binding instance?
                return new TopLevelBinding(requiredBinding);

                // TODO FUSION-117 Just return requiredBinding.myBinding?
                // That doesn't work after namespace-syntax-introduce
            }

            assert definedBinding.getIdentifier()
                .freeIdentifierEqual(requiredBinding.myBinding.getIdentifier());

            if (definedBinding.myPrecedence <= requiredBinding.myPrecedence)
            {
                definedBinding.myTarget = requiredBinding.myBinding;
            }

            return definedBinding;
        }

        @Override
        Iterator<SyntaxWrap> iterator()
        {
            return null;
        }

        @Override
        public String toString()
        {
            return "{{{TopLevelWrap}}}";
        }
    }


    static final class TopLevelRequireBinding
        implements Binding
    {
        private final int myPrecedence;
        private final ModuleBinding myBinding;

        private TopLevelRequireBinding(int precedence,
                                       ModuleBinding original)
        {
            assert original.originalBinding() == original;
            myPrecedence = precedence;
            myBinding = original;
        }

        @Override
        public final String getName()
        {
            return myBinding.getName();
        }

        @Override
        public boolean isFree(String name)
        {
            return false;
        }

        @Override
        public Binding originalBinding()
        {
            return myBinding;
        }

        @Override
        public boolean sameTarget(Binding other)
        {
            return myBinding == other.originalBinding();
        }

        @Override
        public Object lookup(Environment store)
        {
            return myBinding.lookup(store);
        }

        @Override
        public CompiledForm compileReference(Evaluator eval, Environment env)
            throws FusionException
        {
            return myBinding.compileReference(eval, env);
        }

        @Override
        public CompiledForm compileTopReference(Evaluator eval,
                                                Environment env,
                                                SyntaxSymbol id)
            throws FusionException
        {
            String message =
                "#%top not implemented for top-require binding: " + this;
            throw new IllegalStateException(message);
        }

        @Override
        public CompiledForm compileSet(Evaluator eval, Environment env,
                                       CompiledForm valueForm)
            throws FusionException
        {
            return myBinding.compileSet(eval, env, valueForm);
        }

        @Override
        public boolean equals(Object other)
        {
            throw new UnsupportedOperationException();
        }

        @Override
        public String toString()
        {
            return "{{{TopLevelRequireBinding "
                 + myBinding.myModuleId.internString()
                 + ' ' + getName() + "}}}";
        }
    }


    /**
     * Wrap for top-level {@code require}s that keeps track of precedence and
     * annotates bindings accordingly.
     */
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
            ModuleBinding local = localResolveMaybe(name);
            if (local != null)
            {
                // TODO cache these?
                return new TopLevelRequireBinding(myPrecedence, local);
            }

            if (moreWraps.hasNext())
            {
                SyntaxWrap nextWrap = moreWraps.next();
                return nextWrap.resolve(name, moreWraps, returnMarks);
            }

            return null;
        }
    }


    /**
     * A sequence of {@link TopLevelRequireWrap}s for our required modules.
     * This variable is updated after each `require`.
     */
    private SyntaxWraps myRequiredModuleWraps;

    /**
     * Every `require` gets its own precedence level. Definitions win over
     * imports at the same precedence.
     * TODO not true, but it should be!
     * Currently broken for defns that precede any `require`
     */
    private int myCurrentPrecedence = 0;


    /**
     * Constructs a top-level namespace. Any bindings will need to be
     * {@code require}d or {@code define}d.
     */
    TopLevelNamespace(ModuleRegistry registry)
    {
        super(registry, new SyntaxWrap[0]);
        myRequiredModuleWraps = SyntaxWraps.make();
        addWrap(new TopLevelWrap(this));
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
        identifier = identifier.copyAndResolveTop();

        Binding binding = localResolve(identifier);
        if (binding == null)
        {
            binding = addBinding(identifier);
        }
        else
        {
            TopLevelBinding topBinding = (TopLevelBinding) binding;

            // Give this pre-existing binding precedence over any imports that
            // may have happened since the last time it was defined.
            topBinding.myPrecedence = myCurrentPrecedence;

            // Swing the namespace mapping back to this definition
            topBinding.myTarget = topBinding;
        }

        return identifier.copyReplacingBinding(binding);
    }


    @Override
    void require(ModuleInstance module)
        throws FusionException
    {
        SyntaxWrap wrap = new TopLevelRequireWrap(module, myCurrentPrecedence);
        myRequiredModuleWraps = myRequiredModuleWraps.addWrap(wrap);
        myCurrentPrecedence++;
    }
}
