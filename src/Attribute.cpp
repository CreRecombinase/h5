#include "Attribute.h"
#include "Helpers.h"


using namespace Rcpp;
using namespace H5;
using namespace std;

// [[Rcpp::export]]
XPtr<H5::Attribute> CreateAttribute_H5File(XPtr<H5::H5File> loc, string attributename,
		char datatype, NumericVector dimensions, int size) {

	try {
		vector<hsize_t> dims(dimensions.begin(), dimensions.end());
		DataSpace dataspace (dimensions.length(), &dims[0]);

		if(size > 0) { // Adjust for null-termination character
		  size += 1;
		}

		DataType dtype = GetDataType(GetTypechar(datatype), size);

		Attribute attr =  loc->createAttribute(attributename, dtype, dataspace, H5P_DEFAULT);
		return XPtr<H5::Attribute>(new Attribute(attr));
	  } catch (Exception& error) {
		  throw Rcpp::exception("Creation of Attribute failed. Maybe attribute with same name is already existing at location.");
	  }
}

// [[Rcpp::export]]
XPtr<H5::Attribute> CreateAttribute_Group(XPtr<H5::Group> loc, string attributename,
		char datatype, NumericVector dimensions, int size) {

	try {
		vector<hsize_t> dims(dimensions.begin(), dimensions.end());
		DataSpace dataspace (dimensions.length(), &dims[0]);

		if(size > 0) { // Adjust for null-termination character
		  size += 1;
		}

		DataType dtype = GetDataType(GetTypechar(datatype), size);

		Attribute attr =  loc->createAttribute(attributename, dtype, dataspace, H5P_DEFAULT);
		return XPtr<H5::Attribute>(new Attribute(attr));
	  } catch (Exception& error) {
		  throw Rcpp::exception("Creation of Attribute failed. Maybe attribute with same name is already existing at location.");
	  }
}

// [[Rcpp::export]]
XPtr<H5::Attribute> CreateAttribute_DataSet(XPtr<H5::DataSet> loc, string attributename,
		char datatype, NumericVector dimensions, int size) {

	try {
		vector<hsize_t> dims(dimensions.begin(), dimensions.end());
		DataSpace dataspace (dimensions.length(), &dims[0]);

		if(size > 0) { // Adjust for null-termination character
		  size += 1;
		}

		DataType dtype = GetDataType(GetTypechar(datatype), size);

		Attribute attr =  loc->createAttribute(attributename, dtype, dataspace, H5P_DEFAULT);
		return XPtr<H5::Attribute>(new Attribute(attr));
	  } catch (Exception& error) {
		  throw Rcpp::exception("Creation of Attribute failed. Maybe attribute with same name is already existing at location.");
	  }
}

// [[Rcpp::export]]
bool WriteAttribute(XPtr<H5::Attribute> attribute, SEXP mat,
		char datatype, NumericVector count) {
  try {
	size_t stsize = -1;
    DataType dsettype = attribute->getDataType();
    if (!H5Tis_variable_str(dsettype.getId())) {
	  stsize = dsettype.getSize();
    }
    DTYPE dtype = GetTypechar(datatype);
    const void *buf = ConvertBuffer(mat, dtype, stsize);
    attribute->write(GetDataType(dtype, stsize), buf);
    return TRUE;
  } catch(Exception& error) {
    string msg = error.getDetailMsg() + " in " + error.getFuncName();
    throw Rcpp::exception(msg.c_str());
  }
}

// [[Rcpp::export]]
SEXP ReadAttribute(XPtr<H5::Attribute> attribute, NumericVector count) {
  try {
    DataType dtype = attribute->getDataType();

    NumericVector count_rev = clone<NumericVector>(count);
    std::reverse(count_rev.begin(), count_rev.end());

    SEXP data = AllocateRData(dtype, count);
    data = ReadRDataAttribute(dtype, data, attribute);
    return data;
  } catch(Exception& error) {
    string msg = error.getDetailMsg() + " in " + error.getFuncName();
    throw Rcpp::exception(msg.c_str());
  }
}

// [[Rcpp::export]]
bool CloseAttribute(XPtr<H5::Attribute> attribute) {
  attribute->close();
  return true;
}

// [[Rcpp::export]]
XPtr<H5::Attribute> OpenAttribute_H5File(XPtr<H5::H5File> loc, string attributename) {
	try {
		Attribute attr = loc->openAttribute(attributename);
		return XPtr<H5::Attribute>(new Attribute(attr));
	} catch (Exception& error) {
	    throw Rcpp::exception("Opening Attribute failed.");
	}
}

// [[Rcpp::export]]
XPtr<H5::Attribute> OpenAttribute_Group(XPtr<H5::Group> loc, string attributename) {
	try {
			Attribute attr = loc->openAttribute(attributename);
			return XPtr<H5::Attribute>(new Attribute(attr));
		} catch (Exception& error) {
		    throw Rcpp::exception("Opening Attribute failed.");
		}
}

// [[Rcpp::export]]
XPtr<H5::Attribute> OpenAttribute_DataSet(XPtr<H5::DataSet> loc, string attributename) {
	try {
			Attribute attr = loc->openAttribute(attributename);
			return XPtr<H5::Attribute>(new Attribute(attr));
		} catch (Exception& error) {
		    throw Rcpp::exception("Opening Attribute failed.");
		}
}

// [[Rcpp::export]]
char GetAttributeType(XPtr<H5::Attribute> attribute) {
  try {
    DataType dtype = attribute->getDataType();
    return GetTypechar(dtype);
  } catch(Exception& error) {
    string msg = error.getDetailMsg() + " in " + error.getFuncName();
    throw Rcpp::exception(msg.c_str());
  }
}

// [[Rcpp::export]]
NumericVector GetAttributeDimensions(XPtr<H5::Attribute> attribute) {
	try {
	  DataSpace dataspace = attribute->getSpace();
	  int ndim = dataspace.getSimpleExtentNdims();

	  NumericVector out;
	  if(ndim > 0) {
		vector<hsize_t> dims_out(ndim);
		dataspace.getSimpleExtentDims(&dims_out[0], NULL);
		out = NumericVector(dims_out.begin(), dims_out.end());
	  } else { // Assume scalar Attribute
		out = NumericVector(1);
		out[0] = 1;
	  }
	  return out;
	} catch (Exception& error) {
		 string msg = error.getDetailMsg() + " in " + error.getFuncName();
		 throw Rcpp::exception(msg.c_str());
	}
}

// [[Rcpp::export]]
CharacterVector GetAttributeNames_CommonFG(XPtr<H5::CommonFG> file) {
	try {
		CharacterVector(out);
		H5Aiterate2(file->getLocId(), H5_INDEX_NAME, H5_ITER_INC, NULL, attr_info, &out);
		return out;
	} catch (Exception& error) {
		 string msg = error.getDetailMsg() + " in " + error.getFuncName();
		 throw Rcpp::exception(msg.c_str());
	}
}

// [[Rcpp::export]]
CharacterVector GetAttributeNames_DataSet(XPtr<H5::DataSet> file) {
	try {
		CharacterVector(out);
		H5Aiterate2(file->getId(), H5_INDEX_NAME, H5_ITER_INC, NULL, attr_info, &out);
		return out;
	} catch (Exception& error) {
		 string msg = error.getDetailMsg() + " in " + error.getFuncName();
		 throw Rcpp::exception(msg.c_str());
	}
}

herr_t attr_info(hid_t loc_id, const char *name, const H5A_info_t *ainfo, void *opdata) {
//attr_info(hid_t loc_id, const char *name, void *opdata) {
	try {
		((CharacterVector *) opdata)->push_back(name);
		return 0;
	 } catch (Exception& error) {
		 return 1;
	 }
}
