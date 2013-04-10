// Copyright (c) 2012-2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

/**
 * Procedures for working with namespaces.
 */
final class FusionNamespace
{
    private FusionNamespace() { }

    /**
     *
     * @param language must be an absolute module path,
     * starting with {@code "/"}.
     * @param registry may be null, in which case the current namespace's
     * registry is used.
     *
     * @return a fresh namespace
     */
    static Object makeNamespaceWithLanguage(Evaluator eval,
                                            String language,
                                            ModuleRegistry registry)
        throws FusionException
    {
        assert language.startsWith("/");

        // TODO FUSION-74 Resolver should handle absolute paths
        language = language.substring(1);

        if (registry == null)
        {
            registry = eval.findCurrentNamespace().getRegistry();
        }

        Namespace ns = new TopLevelNamespace(registry);
        ns.use(eval, language);
        return ns;
    }


    static final class MakeNamespaceWithLanguageProc
        extends Procedure1
    {
        MakeNamespaceWithLanguageProc()
        {
            //    "                                                                               |
            super("Makes a fresh namespace with the bindings of `language`, which must be a string\n" +
                  "holding an absolute module path.",
                  "language");
        }

        @Override
        Object doApply(Evaluator eval, Object arg) throws FusionException
        {
            String language = checkStringArg(0, arg);
            if (! (language.startsWith("/") && 1 < language.length()))
            {
                throw argFailure("absolute module path", 0, arg);
            }

            return makeNamespaceWithLanguage(eval, language, null);
        }
    }
}
