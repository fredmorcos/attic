////////////////////////////////////////////////////////////////////////////////
//3456789 123456789 123456789 123456789 123456789 123456789 123456789 123456789 
//
//  lowfat - an "engine" for natural document viewing for free desktop-systems
//
//  copyright (c) 2007 Mirco Müller
//
//  lowfat is free software; you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation; either version 2 of the License, or
//  (at your option) any later version.
//
//  lowfat is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with Foobar; if not, write to the Free Software
//  Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
//
////////////////////////////////////////////////////////////////////////////////

#ifndef _SHAREDPTR_H
#define _SHAREDPTR_H

#include <iostream>

template<class T>
class SharedPtr
{
	public:
		T* pData_;
		typedef long ref_cnt_type;
		mutable ref_cnt_type* pRefCnt_;

		void
		addRef () const
		{
			if (pData_)
				++(*pRefCnt_);
		}

		void
		release ()
		{
			if (pData_)
			{
				if (!--(*pRefCnt_))
				{
					delete pData_;
					delete pRefCnt_;
					pData_ = 0;
					pRefCnt_ = 0;
				}
			}
		}

	public:
		typedef T obj_type;

		explicit
		SharedPtr (T* pData = 0)
			: pData_ (pData),
			  pRefCnt_ (pData_ ? new long (1) : 0)
		{
		}
    
		template<class Base>
		SharedPtr(const SharedPtr<Base>& other)
			: pData_ (other.pData_),
			  pRefCnt_ (other.pRefCnt_)
		{
			addRef ();
		}

    SharedPtr(const SharedPtr<T>& rhs) :
        pData_(rhs.pData_), pRefCnt_(rhs.pRefCnt_) {
        addRef();
    }

		~SharedPtr()
		{
			release ();
		}

		bool
		operator==(const SharedPtr<T>& rhs)
		{
			return pData_ == rhs.pData_;
		}

		bool
		operator==(const SharedPtr<T>& rhs) const
		{
			return pData_ == rhs.pData_; 
		}

		SharedPtr<T>&
		operator=(const SharedPtr<T>& rhs)
		{
			rhs.addRef ();
			release ();
			pData_ = rhs.pData_;
			pRefCnt_ = rhs.pRefCnt_;
			return *this;
		}

		bool
		isNull() const
		{
			return pData_ == 0;
		}

		T&
		operator*()
		{
			return *pData_;
		}

		T&
		operator*() const
		{
			return *pData_;
		}

		T*
		operator->()
		{
			return pData_;
		}

		T*
		operator->() const
		{
			return pData_;
		}

		T*
		get () const
		{
			return pData_;
		}

		static SharedPtr null;
};

#endif  // _SHAREDPTR_H
