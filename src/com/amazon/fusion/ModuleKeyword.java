// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonSexp;
import com.amazon.ion.IonSymbol;
import java.io.File;

/**
 *
 */
final class ModuleKeyword
    extends KeywordValue
{
    private final DynamicParameter myCurrentModuleDeclareName;

    ModuleKeyword(DynamicParameter currentModuleDeclareName)
    {
        //    "                                                                               |
        super("NAME BODY ...+",
              "Declares a module containing the given body. The NAME must be a symbol.");

        myCurrentModuleDeclareName = currentModuleDeclareName;
    }

    @Override
    FusionValue invoke(Evaluator eval, Environment env, IonSexp expr)
        throws FusionException
    {
        IonSymbol name = requiredSymbol("module name", 1, expr);
        String declaredName = name.stringValue();
        // TODO check null/empty

        Namespace ns = env.namespace();
        Namespace moduleNamespace = eval.newBaseNamespace(ns);

        for (int i = 2; i < expr.size(); i++)
        {
            eval.eval(moduleNamespace, expr.get(i));
        }

        ModuleIdentity id = determineIdentity(eval, declaredName);
        return new ModuleInstance(id, moduleNamespace);
    }


    private ModuleIdentity determineIdentity(Evaluator eval,
                                             String declaredName)
        throws FusionException
    {
        ModuleIdentity id;
        String current = myCurrentModuleDeclareName.asString(eval);
        if (current != null)
        {
            File file = new File(current);
            id = ModuleIdentity.intern(file);
        }
        else
        {
            id = ModuleIdentity.intern(declaredName);
        }
        return id;
    }
}
