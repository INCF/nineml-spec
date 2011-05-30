/*
 *  ring_buffer.cpp
 *
 *  This file is part of NEST
 *
 *  Copyright (C) 2004 by
 *  The NEST Initiative
 *
 *  See the file AUTHORS for details.
 *
 *  Permission is granted to compile and modify
 *  this file for non-commercial use.
 *  See the file LICENSE for details.
 *
 */

#include "ring_buffer.h"
                
nest::RingBuffer::RingBuffer()
  : buffer_(0.0, Scheduler::get_min_delay()+Scheduler::get_max_delay())
{}

void nest::RingBuffer::resize()
{
  size_t size = Scheduler::get_min_delay()+Scheduler::get_max_delay();
  if (buffer_.size() != size)
  {
    buffer_.resize(size);
    buffer_ = 0.0;
  }
}

void nest::RingBuffer::clear()
{
  resize();    // does nothing if size is fine
  buffer_=0.0; // clear all elements
}




nest::MultRBuffer::MultRBuffer()
  : buffer_(0.0, Scheduler::get_min_delay()+Scheduler::get_max_delay())
{}

void nest::MultRBuffer::resize()
{
  size_t size = Scheduler::get_min_delay()+Scheduler::get_max_delay();
  if (buffer_.size() != size)
  {
    buffer_.resize(size);
    buffer_ = 0.0;
  }
}

void nest::MultRBuffer::clear()
{
  buffer_=0.0;
}





nest::ListRingBuffer::ListRingBuffer()
  : buffer_(Scheduler::get_min_delay()+Scheduler::get_max_delay())
{}

void nest::ListRingBuffer::resize()
{
  size_t size = Scheduler::get_min_delay()+Scheduler::get_max_delay();
  if (buffer_.size() != size)
  {
    buffer_.resize(size);
  }
}

void nest::ListRingBuffer::clear()
{
  resize();    // does nothing if size is fine
  // clear all elements
  for (int i=0;i<buffer_.size();i++) {
    buffer_[i].clear(); 
  }
}

