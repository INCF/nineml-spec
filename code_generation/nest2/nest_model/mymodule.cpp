/*
 *  mymodule.cpp
 *  This file is part of NEST.
 *
 *  Copyright (C) 2008 by
 *  The NEST Initiative
 *
 *  See the file AUTHORS for details.
 *
 *  Permission is granted to compile and modify
 *  this file for non-commercial use.
 *  See the file LICENSE for details.
 *
 */

// include necessary NEST headers
#include "config.h"
#include "network.h"
#include "model.h"
#include "dynamicloader.h"
#include "genericmodel.h"
#include "generic_connector.h"
#include "booldatum.h"
#include "integerdatum.h"
#include "tokenarray.h"
#include "exceptions.h"
#include "sliexceptions.h"
#include "nestmodule.h"

// include headers with your own stuff
#include "mymodule.h"
#include "nest_9ml_neuron.h"

// -- Interface to dynamic module loader ---------------------------------------

/*
 * The dynamic module loader must be able to find your module. 
 * You make the module known to the loader by defining an instance of your 
 * module class in global scope. This instance must have the name
 *
 * <modulename>_LTX_mod
 *
 * The dynamicloader can then load modulename and search for symbol "mod" in it.
 */
 
mynest::MyModule mymodule_LTX_mod;

// -- DynModule functions ------------------------------------------------------

mynest::MyModule::MyModule()
  { 
#ifdef LINKED_MODULE
     // register this module at the dynamic loader
     // this is needed to allow for linking in this module at compile time
     // all registered modules will be initialized by the main app's dynamic loader
     nest::DynamicLoaderModule::registerLinkedModule(this);
#endif     
   }

mynest::MyModule::~MyModule()
   {
   }

   const std::string mynest::MyModule::name(void) const
   {
     return std::string("My NEST Module"); // Return name of the module
   }

  const std::string mynest::MyModule::commandstring(void) const
   {
     /* 1. Tell interpreter that we provide the C++ part of MyModule with the
           current revision number. 
        2. Instruct the interpreter to check that mymodule-init.sli exists, 
           provides at least version 1.0 of the SLI interface to MyModule, and
           to load it.
      */
     return std::string(
       "/mymodule /C++ ($Revision: 8512 $) provide-component "
       "/mymodule /SLI (7165) require-component"
       );
   }




  void mynest::MyModule::init(SLIInterpreter *i, nest::Network*)
  {
    /* Register a neuron or device model.
       Give node type as template argument and the name as second argument.
       The first argument is always a reference to the network.
       Return value is a handle for later unregistration.
    */
    nest::register_model<nest_nineml::iaf_cond_exp_9ml>(nest::NestModule::get_network(), 
                                        "iaf_cond_exp_9ml");

    /* Register a synapse type.
       Give synapse type as template argument and the name as second argument.
       The first argument is always a reference to the network.
    */
    //nest::register_prototype_connection<DropOddSpikeConnection>(nest::NestModule::get_network(), 
    //"drop_odd_synapse");


  }  // MyModule::init()

 
