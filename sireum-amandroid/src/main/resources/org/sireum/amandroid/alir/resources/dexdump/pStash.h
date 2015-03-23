

// **** PStash will act as a dynamic array of elements ; we took most of it from Bruce Eckel's chapter 13 *****
// **** note that the first letter p has nothing to do with pilar ******** 

#ifndef PSTASH_H
#define PSTASH_H

#include <string.h>

class PStash {
	  int quantity; // Number of storage spaces
      int next; // Next empty space
      // Pointer storage:
      void** storage;
	  void inflate(int increase);

	  public:
	  PStash() : quantity(0), next(0), storage(0) {}
	  ~PStash();
	  int add(void* element);
	  void* operator[](int index) const; // Fetch
	  // Remove the reference from this PStash:
	  void* remove(int index);
	  // Number of elements in Stash:
	  int count() const { return next; }
};


int PStash::add(void* element) {
	  const int inflateSize = 10;
      if(next >= quantity)
		 inflate(inflateSize);
	  storage[next++] = element;
	  return(next - 1); // Index number
}

// No ownership:
PStash::~PStash() {
  for(int i = 0; i < next; i++)
	    if(storage[i]) 
		  { 
			  // printf(" \n storage[i] is not yet cleaned up; ideally we cannot clean it here because we do not know the type; so, possible memory leakage; check; \n");
              // delete storage[i]; // this will have undefined behavior because storage[i] is void*;
          }
		delete []storage; 
}

// Operator overloading replacement for fetch
void* PStash::operator[](int index) const {
	  assert(index >= 0); // otherwise, "PStash::operator[] index negative";
	  if(index >= next)
		return 0; // To indicate the end

	  // Produce pointer to desired element:
	  return storage[index];
}

void* PStash::remove(int index) {
	  void* v = operator[](index);
	  // "Remove" the pointer:
	  if(v != 0) 
		  storage[index] = 0;
	  return v;
}

void PStash::inflate(int increase) {
	  const int psz = sizeof(void*);
	  void** st = new void*[quantity + increase];
	  memset(st, 0, (quantity + increase) * psz);
	  memcpy(st, storage, quantity * psz);
	  quantity += increase;
	  delete []storage; // Old storage
	  storage = st; // Point to new memory
} 

#endif // PSTASH_H //
