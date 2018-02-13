// Copyright (c) 2012-2018 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

/**
 * Eval-time storage for modules.
 */
final class ModuleStore
    implements NamespaceStore
{
    private final ModuleRegistry myRegistry;
    private final ModuleStore[]  myRequiredModules;
    private final Object[]       myValues;
    private final BindingDoc[]   myBindingDocs;


    ModuleStore(ModuleRegistry registry,
                ModuleStore[]  requiredModules,
                int            variableCount,
                BindingDoc[]   bindingDocs)
    {
        myRegistry = registry;
        myRequiredModules = requiredModules;
        myValues = new Object[variableCount];
        myBindingDocs = bindingDocs;
    }

    ModuleStore(ModuleRegistry registry,
                Object[] values,
                BindingDoc[] bindingDocs)
    {
        myRegistry = registry;
        myRequiredModules = new ModuleStore[0];
        myValues = values;
        myBindingDocs = bindingDocs;
    }

    @Override
    public Object lookup(int rib, int address)
    {
        throw new IllegalStateException("Rib not found");
    }

    @Override
    public void set(int rib, int address, Object value)
    {
        throw new IllegalStateException("Rib not found");
    }


    @Override
    public ModuleRegistry getRegistry()
    {
        return myRegistry;
    }

    @Override
    public NamespaceStore namespace()
    {
        return this;
    }

    @Override
    public void set(int address, Object value)
    {
        myValues[address] = value;
    }

    @Override
    public Object lookup(int address)
    {
        return myValues[address];
    }

    @Override
    public ModuleStore lookupRequiredModule(int moduleAddress)
    {
        return myRequiredModules[moduleAddress];
    }

    @Override
    public Object lookupImport(int moduleAddress, int bindingAddress)
    {
        return myRequiredModules[moduleAddress].myValues[bindingAddress];
    }

    BindingDoc document(int address)
    {
        if (address < myBindingDocs.length)
        {
            BindingDoc doc = myBindingDocs[address];
            if (doc != null && doc.getKind() == null)
            {
                Object value = lookup(address);
                if (value instanceof Procedure)
                {
                    doc.setKind(BindingDoc.Kind.PROCEDURE);
                }
                else if (value instanceof SyntacticForm)
                {
                    doc.setKind(BindingDoc.Kind.SYNTAX);
                }
            }
            return doc;
        }
        return null;
    }
}
