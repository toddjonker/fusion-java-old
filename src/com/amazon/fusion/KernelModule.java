// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.ModuleIdentity.intern;


/**
 * The kernel bindings for Fusion, the bare minimum that needs to be done
 * using Java code.  This module isn't for end users.
 */
final class KernelModule
    extends ModuleInstance
{
    static final String NAME = "#%kernel";
    static final ModuleIdentity IDENTITY = intern(NAME);


    KernelModule(Evaluator eval, Namespace ns)
        throws FusionException
    {
        super(IDENTITY, ns);
        inferName(NAME);

        FusionValue userDir =
            eval.newString(System.getProperty("user.dir"));
        DynamicParameter currentDirectory =
            new DynamicParameter(userDir);
        DynamicParameter currentLoadRelativeDirectory =
            new DynamicParameter(UNDEF);
        DynamicParameter currentModuleDeclareName =
            new DynamicParameter(UNDEF);
        LoadHandler loadHandler =
            new LoadHandler(currentLoadRelativeDirectory, currentDirectory);
        ModuleNameResolver resolver =
            new ModuleNameResolver(loadHandler,
                                   currentLoadRelativeDirectory,
                                   currentDirectory,
                                   currentModuleDeclareName);
        ModuleKeyword moduleKeyword =
            new ModuleKeyword(resolver, currentModuleDeclareName);
        EvalFileKeyword evalFile =
            new EvalFileKeyword(loadHandler);

        ns.bind("current_directory", currentDirectory);
        ns.bind("define", new DefineKeyword());
        ns.bind("eval_file", evalFile);
        ns.bind("java_new", new JavaNewProc());
        ns.bind("module", moduleKeyword);
        ns.bind("undef", FusionValue.UNDEF);
        ns.bind("use", new UseKeyword(resolver));
    }
}
