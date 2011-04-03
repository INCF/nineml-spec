/*
 *  mymodule.h
 *
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

#ifndef MYMODULE_H
#define MyMODULE_H

#include "dynmodule.h"
#include "slifunction.h"

namespace nest
{
  class Network;
}

// Put your stuff into your own namespace.
namespace mynest {
  
/**
 * Class defining your model.
 * @note For each model, you must define one such class, with a unique name.
 */
class MyModule : public DynModule
{
public:

  // Interface functions ------------------------------------------
  
  /**
   * @note The constructor registers the module with the dynamic loader. 
   *       Initialization proper is performed by the init() method.
   */
  MyModule();
  
  /**
   * @note The destructor does not do much in modules. Proper "downrigging"
   *       is the responsibility of the unregister() method.
   */
  ~MyModule();

  /**
   * Initialize module by registering models with the network.
   * @param SLIInterpreter* SLI interpreter
   * @param nest::Network*  Network with which to register models
   * @note  Parameter Network is needed for historical compatibility
   *        only.
   */
  void init(SLIInterpreter*, nest::Network*);

  /**
   * Return the name of your model.
   */
  const std::string name(void) const;
  
  /**
   * Return the name of a sli file to execute when mymodule is loaded.
   * This mechanism can be used to define SLI commands associated with your
   * module, in particular, set up type tries for functions you have defined.
   */
  const std::string commandstring(void) const;
     
public:
  
  // Classes implementing your functions -----------------------------
  
  /**
   * Implement a function for a step-pattern-based connection.
   * @note What this function does is described in the SLI documentation
   *       in the cpp file.
   * @note The mangled name indicates this function expects the following
   *       arguments on the stack (bottom first): vector of int, int, 
   *       vector of int, int. 
   * @note You must define a member object in your module class
   *       of the function class. execute() is later invoked on this
   *       member.
   */
  };
} // namespace mynest

#endif
