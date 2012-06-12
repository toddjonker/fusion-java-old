// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonSexp;
import com.amazon.ion.IonSymbol;
import com.amazon.ion.IonType;
import com.amazon.ion.IonValue;
import java.io.File;
import java.util.ArrayList;

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

        ArrayList<IonSexp> provideForms = new ArrayList<IonSexp>();

        for (int i = 2; i < expr.size(); i++)
        {
            IonValue form = expr.get(i);
            IonSexp provide = formIsProvide(form);
            if (provide != null)
            {
                provideForms.add(provide);
            }
            else
            {
                eval.eval(moduleNamespace, form);
            }
        }

        String[] providedNames =
            processProvides(provideForms, moduleNamespace);

        ModuleIdentity id = determineIdentity(eval, declaredName);
        return new ModuleInstance(id, moduleNamespace, providedNames);
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

    private IonSexp formIsProvide(IonValue form)
    {
        if (form.getType() == IonType.SEXP)
        {
            IonSexp sexp = (IonSexp) form;
            if (sexp.size() != 0)
            {
                IonValue first = sexp.get(0);
                if (first.getType() == IonType.SYMBOL
                    && "provide".equals(((IonSymbol) first).stringValue()))
                {
                    return sexp;
                }
            }
        }
        return null;
    }

    /**
     * @param provideForms
     * @param moduleNamespace
     * @return
     */
    private String[] processProvides(ArrayList<IonSexp> provideForms,
                                     Namespace moduleNamespace)
        throws SyntaxFailure
    {
        ArrayList<String> names = new ArrayList<String>();

        for (IonSexp form : provideForms)
        {
            int size = form.size();
            SyntaxChecker check = new SyntaxChecker("provide", form);
            for (int i = 1; i < size; i++)
            {
                String name =
                    check.requiredNonEmptySymbol("bound identifier", i);

                if (moduleNamespace.lookup(name) == null)
                {
                    throw check.failure("name is not bound: " + name);
                }

                names.add(name);
            }
        }

        return names.toArray(new String[names.size()]);
    }

}
