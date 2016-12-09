// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#ifndef RCPP_h5_RCPPEXPORTS_H_GEN_
#define RCPP_h5_RCPPEXPORTS_H_GEN_

#include <Rcpp.h>

namespace h5 {

    using namespace Rcpp;

    namespace {
        void validateSignature(const char* sig) {
            Rcpp::Function require = Rcpp::Environment::base_env()["require"];
            require("h5", Rcpp::Named("quietly") = true);
            typedef int(*Ptr_validate)(const char*);
            static Ptr_validate p_validate = (Ptr_validate)
                R_GetCCallable("h5", "h5_RcppExport_validate");
            if (!p_validate(sig)) {
                throw Rcpp::function_not_exported(
                    "C++ function with signature '" + std::string(sig) + "' not found in h5");
            }
        }
    }

    inline XPtr<H5::Attribute> CreateAttribute_H5File(XPtr<H5::H5File> loc, string attributename, char datatype, NumericVector dimensions, int size) {
        typedef SEXP(*Ptr_CreateAttribute_H5File)(SEXP,SEXP,SEXP,SEXP,SEXP);
        static Ptr_CreateAttribute_H5File p_CreateAttribute_H5File = NULL;
        if (p_CreateAttribute_H5File == NULL) {
            validateSignature("XPtr<H5::Attribute>(*CreateAttribute_H5File)(XPtr<H5::H5File>,string,char,NumericVector,int)");
            p_CreateAttribute_H5File = (Ptr_CreateAttribute_H5File)R_GetCCallable("h5", "h5_CreateAttribute_H5File");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_CreateAttribute_H5File(Rcpp::wrap(loc), Rcpp::wrap(attributename), Rcpp::wrap(datatype), Rcpp::wrap(dimensions), Rcpp::wrap(size));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<XPtr<H5::Attribute> >(rcpp_result_gen);
    }

    inline XPtr<H5::Attribute> CreateAttribute_Group(XPtr<H5::Group> loc, string attributename, char datatype, NumericVector dimensions, int size) {
        typedef SEXP(*Ptr_CreateAttribute_Group)(SEXP,SEXP,SEXP,SEXP,SEXP);
        static Ptr_CreateAttribute_Group p_CreateAttribute_Group = NULL;
        if (p_CreateAttribute_Group == NULL) {
            validateSignature("XPtr<H5::Attribute>(*CreateAttribute_Group)(XPtr<H5::Group>,string,char,NumericVector,int)");
            p_CreateAttribute_Group = (Ptr_CreateAttribute_Group)R_GetCCallable("h5", "h5_CreateAttribute_Group");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_CreateAttribute_Group(Rcpp::wrap(loc), Rcpp::wrap(attributename), Rcpp::wrap(datatype), Rcpp::wrap(dimensions), Rcpp::wrap(size));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<XPtr<H5::Attribute> >(rcpp_result_gen);
    }

    inline XPtr<H5::Attribute> CreateAttribute_DataSet(XPtr<H5::DataSet> loc, string attributename, char datatype, NumericVector dimensions, int size) {
        typedef SEXP(*Ptr_CreateAttribute_DataSet)(SEXP,SEXP,SEXP,SEXP,SEXP);
        static Ptr_CreateAttribute_DataSet p_CreateAttribute_DataSet = NULL;
        if (p_CreateAttribute_DataSet == NULL) {
            validateSignature("XPtr<H5::Attribute>(*CreateAttribute_DataSet)(XPtr<H5::DataSet>,string,char,NumericVector,int)");
            p_CreateAttribute_DataSet = (Ptr_CreateAttribute_DataSet)R_GetCCallable("h5", "h5_CreateAttribute_DataSet");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_CreateAttribute_DataSet(Rcpp::wrap(loc), Rcpp::wrap(attributename), Rcpp::wrap(datatype), Rcpp::wrap(dimensions), Rcpp::wrap(size));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<XPtr<H5::Attribute> >(rcpp_result_gen);
    }

    inline bool WriteAttribute(XPtr<H5::Attribute> attribute, SEXP mat, char datatype, NumericVector count) {
        typedef SEXP(*Ptr_WriteAttribute)(SEXP,SEXP,SEXP,SEXP);
        static Ptr_WriteAttribute p_WriteAttribute = NULL;
        if (p_WriteAttribute == NULL) {
            validateSignature("bool(*WriteAttribute)(XPtr<H5::Attribute>,SEXP,char,NumericVector)");
            p_WriteAttribute = (Ptr_WriteAttribute)R_GetCCallable("h5", "h5_WriteAttribute");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_WriteAttribute(Rcpp::wrap(attribute), Rcpp::wrap(mat), Rcpp::wrap(datatype), Rcpp::wrap(count));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<bool >(rcpp_result_gen);
    }

    inline SEXP ReadAttribute(XPtr<H5::Attribute> attribute, NumericVector count) {
        typedef SEXP(*Ptr_ReadAttribute)(SEXP,SEXP);
        static Ptr_ReadAttribute p_ReadAttribute = NULL;
        if (p_ReadAttribute == NULL) {
            validateSignature("SEXP(*ReadAttribute)(XPtr<H5::Attribute>,NumericVector)");
            p_ReadAttribute = (Ptr_ReadAttribute)R_GetCCallable("h5", "h5_ReadAttribute");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_ReadAttribute(Rcpp::wrap(attribute), Rcpp::wrap(count));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<SEXP >(rcpp_result_gen);
    }

    inline bool CloseAttribute(XPtr<H5::Attribute> attribute) {
        typedef SEXP(*Ptr_CloseAttribute)(SEXP);
        static Ptr_CloseAttribute p_CloseAttribute = NULL;
        if (p_CloseAttribute == NULL) {
            validateSignature("bool(*CloseAttribute)(XPtr<H5::Attribute>)");
            p_CloseAttribute = (Ptr_CloseAttribute)R_GetCCallable("h5", "h5_CloseAttribute");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_CloseAttribute(Rcpp::wrap(attribute));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<bool >(rcpp_result_gen);
    }

    inline XPtr<H5::Attribute> OpenAttribute_H5File(XPtr<H5::H5File> loc, string attributename) {
        typedef SEXP(*Ptr_OpenAttribute_H5File)(SEXP,SEXP);
        static Ptr_OpenAttribute_H5File p_OpenAttribute_H5File = NULL;
        if (p_OpenAttribute_H5File == NULL) {
            validateSignature("XPtr<H5::Attribute>(*OpenAttribute_H5File)(XPtr<H5::H5File>,string)");
            p_OpenAttribute_H5File = (Ptr_OpenAttribute_H5File)R_GetCCallable("h5", "h5_OpenAttribute_H5File");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_OpenAttribute_H5File(Rcpp::wrap(loc), Rcpp::wrap(attributename));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<XPtr<H5::Attribute> >(rcpp_result_gen);
    }

    inline XPtr<H5::Attribute> OpenAttribute_Group(XPtr<H5::Group> loc, string attributename) {
        typedef SEXP(*Ptr_OpenAttribute_Group)(SEXP,SEXP);
        static Ptr_OpenAttribute_Group p_OpenAttribute_Group = NULL;
        if (p_OpenAttribute_Group == NULL) {
            validateSignature("XPtr<H5::Attribute>(*OpenAttribute_Group)(XPtr<H5::Group>,string)");
            p_OpenAttribute_Group = (Ptr_OpenAttribute_Group)R_GetCCallable("h5", "h5_OpenAttribute_Group");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_OpenAttribute_Group(Rcpp::wrap(loc), Rcpp::wrap(attributename));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<XPtr<H5::Attribute> >(rcpp_result_gen);
    }

    inline XPtr<H5::Attribute> OpenAttribute_DataSet(XPtr<H5::DataSet> loc, string attributename) {
        typedef SEXP(*Ptr_OpenAttribute_DataSet)(SEXP,SEXP);
        static Ptr_OpenAttribute_DataSet p_OpenAttribute_DataSet = NULL;
        if (p_OpenAttribute_DataSet == NULL) {
            validateSignature("XPtr<H5::Attribute>(*OpenAttribute_DataSet)(XPtr<H5::DataSet>,string)");
            p_OpenAttribute_DataSet = (Ptr_OpenAttribute_DataSet)R_GetCCallable("h5", "h5_OpenAttribute_DataSet");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_OpenAttribute_DataSet(Rcpp::wrap(loc), Rcpp::wrap(attributename));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<XPtr<H5::Attribute> >(rcpp_result_gen);
    }

    inline char GetAttributeType(XPtr<H5::Attribute> attribute) {
        typedef SEXP(*Ptr_GetAttributeType)(SEXP);
        static Ptr_GetAttributeType p_GetAttributeType = NULL;
        if (p_GetAttributeType == NULL) {
            validateSignature("char(*GetAttributeType)(XPtr<H5::Attribute>)");
            p_GetAttributeType = (Ptr_GetAttributeType)R_GetCCallable("h5", "h5_GetAttributeType");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_GetAttributeType(Rcpp::wrap(attribute));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<char >(rcpp_result_gen);
    }

    inline NumericVector GetAttributeDimensions(XPtr<H5::Attribute> attribute) {
        typedef SEXP(*Ptr_GetAttributeDimensions)(SEXP);
        static Ptr_GetAttributeDimensions p_GetAttributeDimensions = NULL;
        if (p_GetAttributeDimensions == NULL) {
            validateSignature("NumericVector(*GetAttributeDimensions)(XPtr<H5::Attribute>)");
            p_GetAttributeDimensions = (Ptr_GetAttributeDimensions)R_GetCCallable("h5", "h5_GetAttributeDimensions");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_GetAttributeDimensions(Rcpp::wrap(attribute));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<NumericVector >(rcpp_result_gen);
    }

    inline CharacterVector GetAttributeNames_CommonFG(XPtr<H5::CommonFG> file) {
        typedef SEXP(*Ptr_GetAttributeNames_CommonFG)(SEXP);
        static Ptr_GetAttributeNames_CommonFG p_GetAttributeNames_CommonFG = NULL;
        if (p_GetAttributeNames_CommonFG == NULL) {
            validateSignature("CharacterVector(*GetAttributeNames_CommonFG)(XPtr<H5::CommonFG>)");
            p_GetAttributeNames_CommonFG = (Ptr_GetAttributeNames_CommonFG)R_GetCCallable("h5", "h5_GetAttributeNames_CommonFG");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_GetAttributeNames_CommonFG(Rcpp::wrap(file));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<CharacterVector >(rcpp_result_gen);
    }

    inline CharacterVector GetAttributeNames_DataSet(XPtr<H5::DataSet> file) {
        typedef SEXP(*Ptr_GetAttributeNames_DataSet)(SEXP);
        static Ptr_GetAttributeNames_DataSet p_GetAttributeNames_DataSet = NULL;
        if (p_GetAttributeNames_DataSet == NULL) {
            validateSignature("CharacterVector(*GetAttributeNames_DataSet)(XPtr<H5::DataSet>)");
            p_GetAttributeNames_DataSet = (Ptr_GetAttributeNames_DataSet)R_GetCCallable("h5", "h5_GetAttributeNames_DataSet");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_GetAttributeNames_DataSet(Rcpp::wrap(file));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<CharacterVector >(rcpp_result_gen);
    }

    inline bool WriteDataset(XPtr<DataSet> dataset, XPtr<DataSpace> dataspace, SEXP mat, char datatype, NumericVector count) {
        typedef SEXP(*Ptr_WriteDataset)(SEXP,SEXP,SEXP,SEXP,SEXP);
        static Ptr_WriteDataset p_WriteDataset = NULL;
        if (p_WriteDataset == NULL) {
            validateSignature("bool(*WriteDataset)(XPtr<DataSet>,XPtr<DataSpace>,SEXP,char,NumericVector)");
            p_WriteDataset = (Ptr_WriteDataset)R_GetCCallable("h5", "h5_WriteDataset");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_WriteDataset(Rcpp::wrap(dataset), Rcpp::wrap(dataspace), Rcpp::wrap(mat), Rcpp::wrap(datatype), Rcpp::wrap(count));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<bool >(rcpp_result_gen);
    }

    inline bool ExtendDataset(XPtr<DataSet> dset, NumericVector dimsnew) {
        typedef SEXP(*Ptr_ExtendDataset)(SEXP,SEXP);
        static Ptr_ExtendDataset p_ExtendDataset = NULL;
        if (p_ExtendDataset == NULL) {
            validateSignature("bool(*ExtendDataset)(XPtr<DataSet>,NumericVector)");
            p_ExtendDataset = (Ptr_ExtendDataset)R_GetCCallable("h5", "h5_ExtendDataset");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_ExtendDataset(Rcpp::wrap(dset), Rcpp::wrap(dimsnew));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<bool >(rcpp_result_gen);
    }

    inline char GetDataSetType(XPtr<DataSet> dataset) {
        typedef SEXP(*Ptr_GetDataSetType)(SEXP);
        static Ptr_GetDataSetType p_GetDataSetType = NULL;
        if (p_GetDataSetType == NULL) {
            validateSignature("char(*GetDataSetType)(XPtr<DataSet>)");
            p_GetDataSetType = (Ptr_GetDataSetType)R_GetCCallable("h5", "h5_GetDataSetType");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_GetDataSetType(Rcpp::wrap(dataset));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<char >(rcpp_result_gen);
    }

    inline SEXP ReadDataset(XPtr<DataSet> dataset, XPtr<DataSpace> dataspace, NumericVector count) {
        typedef SEXP(*Ptr_ReadDataset)(SEXP,SEXP,SEXP);
        static Ptr_ReadDataset p_ReadDataset = NULL;
        if (p_ReadDataset == NULL) {
            validateSignature("SEXP(*ReadDataset)(XPtr<DataSet>,XPtr<DataSpace>,NumericVector)");
            p_ReadDataset = (Ptr_ReadDataset)R_GetCCallable("h5", "h5_ReadDataset");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_ReadDataset(Rcpp::wrap(dataset), Rcpp::wrap(dataspace), Rcpp::wrap(count));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<SEXP >(rcpp_result_gen);
    }

    inline bool CloseDataset(XPtr<DataSet> dataset) {
        typedef SEXP(*Ptr_CloseDataset)(SEXP);
        static Ptr_CloseDataset p_CloseDataset = NULL;
        if (p_CloseDataset == NULL) {
            validateSignature("bool(*CloseDataset)(XPtr<DataSet>)");
            p_CloseDataset = (Ptr_CloseDataset)R_GetCCallable("h5", "h5_CloseDataset");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_CloseDataset(Rcpp::wrap(dataset));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<bool >(rcpp_result_gen);
    }

    inline XPtr<DataSet> CreateDataset(XPtr<CommonFG> file, string datasetname, char datatype, NumericVector dimensions, NumericVector chunksize, NumericVector maxshape, int compressionlevel, int size) {
        typedef SEXP(*Ptr_CreateDataset)(SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP,SEXP);
        static Ptr_CreateDataset p_CreateDataset = NULL;
        if (p_CreateDataset == NULL) {
            validateSignature("XPtr<DataSet>(*CreateDataset)(XPtr<CommonFG>,string,char,NumericVector,NumericVector,NumericVector,int,int)");
            p_CreateDataset = (Ptr_CreateDataset)R_GetCCallable("h5", "h5_CreateDataset");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_CreateDataset(Rcpp::wrap(file), Rcpp::wrap(datasetname), Rcpp::wrap(datatype), Rcpp::wrap(dimensions), Rcpp::wrap(chunksize), Rcpp::wrap(maxshape), Rcpp::wrap(compressionlevel), Rcpp::wrap(size));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<XPtr<DataSet> >(rcpp_result_gen);
    }

    inline XPtr<DataSet> OpenDataset(XPtr<CommonFG> file, string datasetname) {
        typedef SEXP(*Ptr_OpenDataset)(SEXP,SEXP);
        static Ptr_OpenDataset p_OpenDataset = NULL;
        if (p_OpenDataset == NULL) {
            validateSignature("XPtr<DataSet>(*OpenDataset)(XPtr<CommonFG>,string)");
            p_OpenDataset = (Ptr_OpenDataset)R_GetCCallable("h5", "h5_OpenDataset");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_OpenDataset(Rcpp::wrap(file), Rcpp::wrap(datasetname));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<XPtr<DataSet> >(rcpp_result_gen);
    }

    inline NumericVector GetDataSetDimensions(XPtr<DataSet> dataset) {
        typedef SEXP(*Ptr_GetDataSetDimensions)(SEXP);
        static Ptr_GetDataSetDimensions p_GetDataSetDimensions = NULL;
        if (p_GetDataSetDimensions == NULL) {
            validateSignature("NumericVector(*GetDataSetDimensions)(XPtr<DataSet>)");
            p_GetDataSetDimensions = (Ptr_GetDataSetDimensions)R_GetCCallable("h5", "h5_GetDataSetDimensions");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_GetDataSetDimensions(Rcpp::wrap(dataset));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<NumericVector >(rcpp_result_gen);
    }

    inline NumericVector GetDataSetMaxDimensions(XPtr<DataSet> dataset) {
        typedef SEXP(*Ptr_GetDataSetMaxDimensions)(SEXP);
        static Ptr_GetDataSetMaxDimensions p_GetDataSetMaxDimensions = NULL;
        if (p_GetDataSetMaxDimensions == NULL) {
            validateSignature("NumericVector(*GetDataSetMaxDimensions)(XPtr<DataSet>)");
            p_GetDataSetMaxDimensions = (Ptr_GetDataSetMaxDimensions)R_GetCCallable("h5", "h5_GetDataSetMaxDimensions");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_GetDataSetMaxDimensions(Rcpp::wrap(dataset));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<NumericVector >(rcpp_result_gen);
    }

    inline NumericVector GetDataSetChunksize(XPtr<DataSet> dataset) {
        typedef SEXP(*Ptr_GetDataSetChunksize)(SEXP);
        static Ptr_GetDataSetChunksize p_GetDataSetChunksize = NULL;
        if (p_GetDataSetChunksize == NULL) {
            validateSignature("NumericVector(*GetDataSetChunksize)(XPtr<DataSet>)");
            p_GetDataSetChunksize = (Ptr_GetDataSetChunksize)R_GetCCallable("h5", "h5_GetDataSetChunksize");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_GetDataSetChunksize(Rcpp::wrap(dataset));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<NumericVector >(rcpp_result_gen);
    }

    inline CharacterVector GetDataSetCompression(XPtr<DataSet> dataset) {
        typedef SEXP(*Ptr_GetDataSetCompression)(SEXP);
        static Ptr_GetDataSetCompression p_GetDataSetCompression = NULL;
        if (p_GetDataSetCompression == NULL) {
            validateSignature("CharacterVector(*GetDataSetCompression)(XPtr<DataSet>)");
            p_GetDataSetCompression = (Ptr_GetDataSetCompression)R_GetCCallable("h5", "h5_GetDataSetCompression");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_GetDataSetCompression(Rcpp::wrap(dataset));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<CharacterVector >(rcpp_result_gen);
    }

    inline XPtr<DataSpace> GetDataspace(XPtr<DataSet> dataset) {
        typedef SEXP(*Ptr_GetDataspace)(SEXP);
        static Ptr_GetDataspace p_GetDataspace = NULL;
        if (p_GetDataspace == NULL) {
            validateSignature("XPtr<DataSpace>(*GetDataspace)(XPtr<DataSet>)");
            p_GetDataspace = (Ptr_GetDataspace)R_GetCCallable("h5", "h5_GetDataspace");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_GetDataspace(Rcpp::wrap(dataset));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<XPtr<DataSpace> >(rcpp_result_gen);
    }

    inline XPtr<DataSpace> SelectHyperslab(XPtr<DataSpace> dataspace, NumericVector offset, NumericVector count, string seloper = "SET") {
        typedef SEXP(*Ptr_SelectHyperslab)(SEXP,SEXP,SEXP,SEXP);
        static Ptr_SelectHyperslab p_SelectHyperslab = NULL;
        if (p_SelectHyperslab == NULL) {
            validateSignature("XPtr<DataSpace>(*SelectHyperslab)(XPtr<DataSpace>,NumericVector,NumericVector,string)");
            p_SelectHyperslab = (Ptr_SelectHyperslab)R_GetCCallable("h5", "h5_SelectHyperslab");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_SelectHyperslab(Rcpp::wrap(dataspace), Rcpp::wrap(offset), Rcpp::wrap(count), Rcpp::wrap(seloper));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<XPtr<DataSpace> >(rcpp_result_gen);
    }

    inline XPtr<DataSpace> SelectElem(XPtr<DataSpace> dataspace, NumericMatrix coords) {
        typedef SEXP(*Ptr_SelectElem)(SEXP,SEXP);
        static Ptr_SelectElem p_SelectElem = NULL;
        if (p_SelectElem == NULL) {
            validateSignature("XPtr<DataSpace>(*SelectElem)(XPtr<DataSpace>,NumericMatrix)");
            p_SelectElem = (Ptr_SelectElem)R_GetCCallable("h5", "h5_SelectElem");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_SelectElem(Rcpp::wrap(dataspace), Rcpp::wrap(coords));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<XPtr<DataSpace> >(rcpp_result_gen);
    }

    inline XPtr<DataSpace> SelectAll(XPtr<DataSpace> dataspace) {
        typedef SEXP(*Ptr_SelectAll)(SEXP);
        static Ptr_SelectAll p_SelectAll = NULL;
        if (p_SelectAll == NULL) {
            validateSignature("XPtr<DataSpace>(*SelectAll)(XPtr<DataSpace>)");
            p_SelectAll = (Ptr_SelectAll)R_GetCCallable("h5", "h5_SelectAll");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_SelectAll(Rcpp::wrap(dataspace));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<XPtr<DataSpace> >(rcpp_result_gen);
    }

    inline bool CloseDataspace(XPtr<DataSpace> dataspace) {
        typedef SEXP(*Ptr_CloseDataspace)(SEXP);
        static Ptr_CloseDataspace p_CloseDataspace = NULL;
        if (p_CloseDataspace == NULL) {
            validateSignature("bool(*CloseDataspace)(XPtr<DataSpace>)");
            p_CloseDataspace = (Ptr_CloseDataspace)R_GetCCallable("h5", "h5_CloseDataspace");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_CloseDataspace(Rcpp::wrap(dataspace));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<bool >(rcpp_result_gen);
    }

    inline XPtr<H5File> OpenFile(string filePath, string mode) {
        typedef SEXP(*Ptr_OpenFile)(SEXP,SEXP);
        static Ptr_OpenFile p_OpenFile = NULL;
        if (p_OpenFile == NULL) {
            validateSignature("XPtr<H5File>(*OpenFile)(string,string)");
            p_OpenFile = (Ptr_OpenFile)R_GetCCallable("h5", "h5_OpenFile");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_OpenFile(Rcpp::wrap(filePath), Rcpp::wrap(mode));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<XPtr<H5File> >(rcpp_result_gen);
    }

    inline bool CloseFile(XPtr<H5File> file) {
        typedef SEXP(*Ptr_CloseFile)(SEXP);
        static Ptr_CloseFile p_CloseFile = NULL;
        if (p_CloseFile == NULL) {
            validateSignature("bool(*CloseFile)(XPtr<H5File>)");
            p_CloseFile = (Ptr_CloseFile)R_GetCCallable("h5", "h5_CloseFile");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_CloseFile(Rcpp::wrap(file));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<bool >(rcpp_result_gen);
    }

    inline bool FlushFile(XPtr<H5File> file) {
        typedef SEXP(*Ptr_FlushFile)(SEXP);
        static Ptr_FlushFile p_FlushFile = NULL;
        if (p_FlushFile == NULL) {
            validateSignature("bool(*FlushFile)(XPtr<H5File>)");
            p_FlushFile = (Ptr_FlushFile)R_GetCCallable("h5", "h5_FlushFile");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_FlushFile(Rcpp::wrap(file));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<bool >(rcpp_result_gen);
    }

    inline bool IsHDF5File(string fname) {
        typedef SEXP(*Ptr_IsHDF5File)(SEXP);
        static Ptr_IsHDF5File p_IsHDF5File = NULL;
        if (p_IsHDF5File == NULL) {
            validateSignature("bool(*IsHDF5File)(string)");
            p_IsHDF5File = (Ptr_IsHDF5File)R_GetCCallable("h5", "h5_IsHDF5File");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_IsHDF5File(Rcpp::wrap(fname));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<bool >(rcpp_result_gen);
    }

    inline XPtr<Group> CreateGroup(XPtr<CommonFG> file, string groupname) {
        typedef SEXP(*Ptr_CreateGroup)(SEXP,SEXP);
        static Ptr_CreateGroup p_CreateGroup = NULL;
        if (p_CreateGroup == NULL) {
            validateSignature("XPtr<Group>(*CreateGroup)(XPtr<CommonFG>,string)");
            p_CreateGroup = (Ptr_CreateGroup)R_GetCCallable("h5", "h5_CreateGroup");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_CreateGroup(Rcpp::wrap(file), Rcpp::wrap(groupname));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<XPtr<Group> >(rcpp_result_gen);
    }

    inline XPtr<Group> OpenGroup(XPtr<CommonFG> file, string groupname) {
        typedef SEXP(*Ptr_OpenGroup)(SEXP,SEXP);
        static Ptr_OpenGroup p_OpenGroup = NULL;
        if (p_OpenGroup == NULL) {
            validateSignature("XPtr<Group>(*OpenGroup)(XPtr<CommonFG>,string)");
            p_OpenGroup = (Ptr_OpenGroup)R_GetCCallable("h5", "h5_OpenGroup");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_OpenGroup(Rcpp::wrap(file), Rcpp::wrap(groupname));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<XPtr<Group> >(rcpp_result_gen);
    }

    inline bool CloseGroup(XPtr<Group> group) {
        typedef SEXP(*Ptr_CloseGroup)(SEXP);
        static Ptr_CloseGroup p_CloseGroup = NULL;
        if (p_CloseGroup == NULL) {
            validateSignature("bool(*CloseGroup)(XPtr<Group>)");
            p_CloseGroup = (Ptr_CloseGroup)R_GetCCallable("h5", "h5_CloseGroup");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_CloseGroup(Rcpp::wrap(group));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<bool >(rcpp_result_gen);
    }

    inline bool ExistsGroup(XPtr<CommonFG> file, string groupname) {
        typedef SEXP(*Ptr_ExistsGroup)(SEXP,SEXP);
        static Ptr_ExistsGroup p_ExistsGroup = NULL;
        if (p_ExistsGroup == NULL) {
            validateSignature("bool(*ExistsGroup)(XPtr<CommonFG>,string)");
            p_ExistsGroup = (Ptr_ExistsGroup)R_GetCCallable("h5", "h5_ExistsGroup");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_ExistsGroup(Rcpp::wrap(file), Rcpp::wrap(groupname));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<bool >(rcpp_result_gen);
    }

    inline CharacterVector GetGroupNames(XPtr<CommonFG> file, string path, bool recursive) {
        typedef SEXP(*Ptr_GetGroupNames)(SEXP,SEXP,SEXP);
        static Ptr_GetGroupNames p_GetGroupNames = NULL;
        if (p_GetGroupNames == NULL) {
            validateSignature("CharacterVector(*GetGroupNames)(XPtr<CommonFG>,string,bool)");
            p_GetGroupNames = (Ptr_GetGroupNames)R_GetCCallable("h5", "h5_GetGroupNames");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_GetGroupNames(Rcpp::wrap(file), Rcpp::wrap(path), Rcpp::wrap(recursive));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<CharacterVector >(rcpp_result_gen);
    }

    inline CharacterVector GetDataSetNames(XPtr<CommonFG> file, string path, bool recursive) {
        typedef SEXP(*Ptr_GetDataSetNames)(SEXP,SEXP,SEXP);
        static Ptr_GetDataSetNames p_GetDataSetNames = NULL;
        if (p_GetDataSetNames == NULL) {
            validateSignature("CharacterVector(*GetDataSetNames)(XPtr<CommonFG>,string,bool)");
            p_GetDataSetNames = (Ptr_GetDataSetNames)R_GetCCallable("h5", "h5_GetDataSetNames");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_GetDataSetNames(Rcpp::wrap(file), Rcpp::wrap(path), Rcpp::wrap(recursive));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<CharacterVector >(rcpp_result_gen);
    }

    inline CharacterVector GetSoftLinks(XPtr<CommonFG> file, string path) {
        typedef SEXP(*Ptr_GetSoftLinks)(SEXP,SEXP);
        static Ptr_GetSoftLinks p_GetSoftLinks = NULL;
        if (p_GetSoftLinks == NULL) {
            validateSignature("CharacterVector(*GetSoftLinks)(XPtr<CommonFG>,string)");
            p_GetSoftLinks = (Ptr_GetSoftLinks)R_GetCCallable("h5", "h5_GetSoftLinks");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_GetSoftLinks(Rcpp::wrap(file), Rcpp::wrap(path));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<CharacterVector >(rcpp_result_gen);
    }

    inline bool Unlink(XPtr<CommonFG> file, string path) {
        typedef SEXP(*Ptr_Unlink)(SEXP,SEXP);
        static Ptr_Unlink p_Unlink = NULL;
        if (p_Unlink == NULL) {
            validateSignature("bool(*Unlink)(XPtr<CommonFG>,string)");
            p_Unlink = (Ptr_Unlink)R_GetCCallable("h5", "h5_Unlink");
        }
        RObject rcpp_result_gen;
        {
            RNGScope RCPP_rngScope_gen;
            rcpp_result_gen = p_Unlink(Rcpp::wrap(file), Rcpp::wrap(path));
        }
        if (rcpp_result_gen.inherits("interrupted-error"))
            throw Rcpp::internal::InterruptedException();
        if (rcpp_result_gen.inherits("try-error"))
            throw Rcpp::exception(as<std::string>(rcpp_result_gen).c_str());
        return Rcpp::as<bool >(rcpp_result_gen);
    }

}

#endif // RCPP_h5_RCPPEXPORTS_H_GEN_
