// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import java.io.File;
import java.util.HashMap;
import java.util.Map;

/**
 * A unique identitier for modules available to the Fusion runtime system.
 * This plays the same role as Racket's "resolved module path".
 */
final class ModuleIdentity
{
    private static final Map<String,ModuleIdentity> ourInternedIdentities =
        new HashMap<String,ModuleIdentity>();

    static ModuleIdentity intern(String name)
    {
        assert ! name.startsWith("/");

        ModuleIdentity interned = ourInternedIdentities.get(name);
        if (interned != null) return interned;

        ModuleIdentity id = new ModuleIdentity(name);
        ourInternedIdentities.put(name, id);
        return id;
    }

    static ModuleIdentity intern(File path)
    {
        // TODO should be canonical path
        assert path.isAbsolute();
        String name = path.getAbsolutePath();

        ModuleIdentity interned = ourInternedIdentities.get(name);
        if (interned != null) return interned;

        ModuleIdentity id = new ModuleIdentity(name);
        ourInternedIdentities.put(name, id);
        return id;
    }

    private final String myName;

    private ModuleIdentity(String name)
    {
        myName = name;
    }


    @Override
    public String toString()
    {
        return myName;
    }


    @Override
    public int hashCode()
    {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((myName == null) ? 0 : myName.hashCode());
        return result;
    }


    @Override
    public boolean equals(Object obj)
    {
        if (this == obj) return true;
        if (obj == null) return false;
        if (getClass() != obj.getClass()) return false;
        ModuleIdentity other = (ModuleIdentity) obj;
        if (myName == null)
        {
            if (other.myName != null) return false;
        }
        else if ( !myName.equals(other.myName)) return false;
        return true;
    }
}
