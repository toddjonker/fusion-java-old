// Copyright (c) 2013 Amazon.com, Inc.  All rights reserved.

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
        implements Binding
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
                "#%top not implemented for language binding: " + this;
            throw new SyntaxFailure("#%top", message, id);
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
            return "{{{LanguageBinding " + myBinding.myModuleId.internString()
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
        assert ! moreWraps.hasNext();
        // If that's not always true we must collect marks from moreWraps.
        // and perhaps look up name there too?

        ModuleBinding local = myModule.resolveProvidedName(name);
        if (local != null)
        {
            return new LanguageBinding(local);
        }

        return null;
    }


    @Override
    Iterator<SyntaxWrap> iterator()
    {
        return null;
    }


    @Override
    public String toString()
    {
        String id = myModule.getIdentity().internString();
        return "{{{Language wrap for " + id + "}}}";
    }
}
