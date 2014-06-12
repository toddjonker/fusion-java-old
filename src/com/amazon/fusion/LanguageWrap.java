// Copyright (c) 2013-2014 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.fusion.ModuleNamespace.ModuleBinding;
import java.util.Iterator;
import java.util.Set;

/**
 * Syntax wrap that adds all bindings provided by a "language" module.
 * We use special bindings so that we can distinguish from those that are
 * introduced via regular imports, since these bindings can be shadowed by
 * such imports and by module-level definitions.
 */
final class LanguageWrap
    extends SyntaxWrap
{
    static final class LanguageBinding
        extends Binding
    {
        private final ModuleBinding myBinding;

        private LanguageBinding(ModuleBinding original)
        {
            assert original.originalBinding() == original;
            myBinding = original;
        }

        @Override
        public final String getName()
        {
            return myBinding.getName();
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
        public Object lookup(Namespace ns)
        {
            return myBinding.lookup(ns);
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
                "#%top not implemented for language binding: " + this;
            throw new SyntaxException("#%top", message, id);
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
            return "{{{LanguageBinding " + myBinding.myModuleId.absolutePath()
                 + ' ' + getName() + "}}}";
        }
    }


    private final ModuleInstance myModule;

    LanguageWrap(ModuleInstance module)
    {
        assert module != null;
        myModule = module;
    }


    @Override
    Binding resolve(String name,
                    Iterator<SyntaxWrap> moreWraps,
                    Set<Integer> returnMarks)
    {
        Binding b = null;
        if (moreWraps.hasNext())
        {
            SyntaxWrap nextWrap = moreWraps.next();
            b = nextWrap.resolve(name, moreWraps, returnMarks);
        }

        // Language-provided bindings never have marks!
        // We should not match unbound macro-introduced identifiers.
        if (b == null && returnMarks.isEmpty())
        {
            ModuleBinding local = myModule.resolveProvidedName(name);
            if (local != null)
            {
                return new LanguageBinding(local);
            }
        }

        return b;
    }


    @Override
    Iterator<SyntaxWrap> iterator()
    {
        return null;
    }


    @Override
    public String toString()
    {
        String id = myModule.getIdentity().absolutePath();
        return "{{{Language wrap for " + id + "}}}";
    }
}
