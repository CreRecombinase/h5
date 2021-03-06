// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include "../inst/include/h5.h"
#include <Rcpp.h>

using namespace Rcpp;

// CreateAttribute_H5File
XPtr<H5::Attribute> CreateAttribute_H5File(XPtr<H5::H5File> loc, string attributename, char datatype, NumericVector dimensions, int size);
RcppExport SEXP h5_CreateAttribute_H5File(SEXP locSEXP, SEXP attributenameSEXP, SEXP datatypeSEXP, SEXP dimensionsSEXP, SEXP sizeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< XPtr<H5::H5File> >::type loc(locSEXP);
    Rcpp::traits::input_parameter< string >::type attributename(attributenameSEXP);
    Rcpp::traits::input_parameter< char >::type datatype(datatypeSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type dimensions(dimensionsSEXP);
    Rcpp::traits::input_parameter< int >::type size(sizeSEXP);
    rcpp_result_gen = Rcpp::wrap(CreateAttribute_H5File(loc, attributename, datatype, dimensions, size));
    return rcpp_result_gen;
END_RCPP
}
// CreateAttribute_Group
XPtr<H5::Attribute> CreateAttribute_Group(XPtr<H5::Group> loc, string attributename, char datatype, NumericVector dimensions, int size);
RcppExport SEXP h5_CreateAttribute_Group(SEXP locSEXP, SEXP attributenameSEXP, SEXP datatypeSEXP, SEXP dimensionsSEXP, SEXP sizeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< XPtr<H5::Group> >::type loc(locSEXP);
    Rcpp::traits::input_parameter< string >::type attributename(attributenameSEXP);
    Rcpp::traits::input_parameter< char >::type datatype(datatypeSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type dimensions(dimensionsSEXP);
    Rcpp::traits::input_parameter< int >::type size(sizeSEXP);
    rcpp_result_gen = Rcpp::wrap(CreateAttribute_Group(loc, attributename, datatype, dimensions, size));
    return rcpp_result_gen;
END_RCPP
}
// CreateAttribute_DataSet
XPtr<H5::Attribute> CreateAttribute_DataSet(XPtr<H5::DataSet> loc, string attributename, char datatype, NumericVector dimensions, int size);
RcppExport SEXP h5_CreateAttribute_DataSet(SEXP locSEXP, SEXP attributenameSEXP, SEXP datatypeSEXP, SEXP dimensionsSEXP, SEXP sizeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< XPtr<H5::DataSet> >::type loc(locSEXP);
    Rcpp::traits::input_parameter< string >::type attributename(attributenameSEXP);
    Rcpp::traits::input_parameter< char >::type datatype(datatypeSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type dimensions(dimensionsSEXP);
    Rcpp::traits::input_parameter< int >::type size(sizeSEXP);
    rcpp_result_gen = Rcpp::wrap(CreateAttribute_DataSet(loc, attributename, datatype, dimensions, size));
    return rcpp_result_gen;
END_RCPP
}
// WriteAttribute
bool WriteAttribute(XPtr<H5::Attribute> attribute, SEXP mat, char datatype, NumericVector count);
RcppExport SEXP h5_WriteAttribute(SEXP attributeSEXP, SEXP matSEXP, SEXP datatypeSEXP, SEXP countSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< XPtr<H5::Attribute> >::type attribute(attributeSEXP);
    Rcpp::traits::input_parameter< SEXP >::type mat(matSEXP);
    Rcpp::traits::input_parameter< char >::type datatype(datatypeSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type count(countSEXP);
    rcpp_result_gen = Rcpp::wrap(WriteAttribute(attribute, mat, datatype, count));
    return rcpp_result_gen;
END_RCPP
}
// ReadAttribute
SEXP ReadAttribute(XPtr<H5::Attribute> attribute, NumericVector count);
RcppExport SEXP h5_ReadAttribute(SEXP attributeSEXP, SEXP countSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< XPtr<H5::Attribute> >::type attribute(attributeSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type count(countSEXP);
    rcpp_result_gen = Rcpp::wrap(ReadAttribute(attribute, count));
    return rcpp_result_gen;
END_RCPP
}
// CloseAttribute
bool CloseAttribute(XPtr<H5::Attribute> attribute);
RcppExport SEXP h5_CloseAttribute(SEXP attributeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< XPtr<H5::Attribute> >::type attribute(attributeSEXP);
    rcpp_result_gen = Rcpp::wrap(CloseAttribute(attribute));
    return rcpp_result_gen;
END_RCPP
}
// OpenAttribute_H5File
XPtr<H5::Attribute> OpenAttribute_H5File(XPtr<H5::H5File> loc, string attributename);
RcppExport SEXP h5_OpenAttribute_H5File(SEXP locSEXP, SEXP attributenameSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< XPtr<H5::H5File> >::type loc(locSEXP);
    Rcpp::traits::input_parameter< string >::type attributename(attributenameSEXP);
    rcpp_result_gen = Rcpp::wrap(OpenAttribute_H5File(loc, attributename));
    return rcpp_result_gen;
END_RCPP
}
// OpenAttribute_Group
XPtr<H5::Attribute> OpenAttribute_Group(XPtr<H5::Group> loc, string attributename);
RcppExport SEXP h5_OpenAttribute_Group(SEXP locSEXP, SEXP attributenameSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< XPtr<H5::Group> >::type loc(locSEXP);
    Rcpp::traits::input_parameter< string >::type attributename(attributenameSEXP);
    rcpp_result_gen = Rcpp::wrap(OpenAttribute_Group(loc, attributename));
    return rcpp_result_gen;
END_RCPP
}
// OpenAttribute_DataSet
XPtr<H5::Attribute> OpenAttribute_DataSet(XPtr<H5::DataSet> loc, string attributename);
RcppExport SEXP h5_OpenAttribute_DataSet(SEXP locSEXP, SEXP attributenameSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< XPtr<H5::DataSet> >::type loc(locSEXP);
    Rcpp::traits::input_parameter< string >::type attributename(attributenameSEXP);
    rcpp_result_gen = Rcpp::wrap(OpenAttribute_DataSet(loc, attributename));
    return rcpp_result_gen;
END_RCPP
}
// GetAttributeType
char GetAttributeType(XPtr<H5::Attribute> attribute);
RcppExport SEXP h5_GetAttributeType(SEXP attributeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< XPtr<H5::Attribute> >::type attribute(attributeSEXP);
    rcpp_result_gen = Rcpp::wrap(GetAttributeType(attribute));
    return rcpp_result_gen;
END_RCPP
}
// GetAttributeDimensions
NumericVector GetAttributeDimensions(XPtr<H5::Attribute> attribute);
RcppExport SEXP h5_GetAttributeDimensions(SEXP attributeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< XPtr<H5::Attribute> >::type attribute(attributeSEXP);
    rcpp_result_gen = Rcpp::wrap(GetAttributeDimensions(attribute));
    return rcpp_result_gen;
END_RCPP
}
// GetAttributeNames_CommonFG
CharacterVector GetAttributeNames_CommonFG(XPtr<H5::CommonFG> file);
RcppExport SEXP h5_GetAttributeNames_CommonFG(SEXP fileSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< XPtr<H5::CommonFG> >::type file(fileSEXP);
    rcpp_result_gen = Rcpp::wrap(GetAttributeNames_CommonFG(file));
    return rcpp_result_gen;
END_RCPP
}
// GetAttributeNames_DataSet
CharacterVector GetAttributeNames_DataSet(XPtr<H5::DataSet> file);
RcppExport SEXP h5_GetAttributeNames_DataSet(SEXP fileSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< XPtr<H5::DataSet> >::type file(fileSEXP);
    rcpp_result_gen = Rcpp::wrap(GetAttributeNames_DataSet(file));
    return rcpp_result_gen;
END_RCPP
}
// WriteDataset
bool WriteDataset(XPtr<DataSet> dataset, XPtr<DataSpace> dataspace, SEXP mat, char datatype, NumericVector count);
RcppExport SEXP h5_WriteDataset(SEXP datasetSEXP, SEXP dataspaceSEXP, SEXP matSEXP, SEXP datatypeSEXP, SEXP countSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< XPtr<DataSet> >::type dataset(datasetSEXP);
    Rcpp::traits::input_parameter< XPtr<DataSpace> >::type dataspace(dataspaceSEXP);
    Rcpp::traits::input_parameter< SEXP >::type mat(matSEXP);
    Rcpp::traits::input_parameter< char >::type datatype(datatypeSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type count(countSEXP);
    rcpp_result_gen = Rcpp::wrap(WriteDataset(dataset, dataspace, mat, datatype, count));
    return rcpp_result_gen;
END_RCPP
}
// ExtendDataset
bool ExtendDataset(XPtr<DataSet> dset, NumericVector dimsnew);
RcppExport SEXP h5_ExtendDataset(SEXP dsetSEXP, SEXP dimsnewSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< XPtr<DataSet> >::type dset(dsetSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type dimsnew(dimsnewSEXP);
    rcpp_result_gen = Rcpp::wrap(ExtendDataset(dset, dimsnew));
    return rcpp_result_gen;
END_RCPP
}
// GetDataSetType
char GetDataSetType(XPtr<DataSet> dataset);
RcppExport SEXP h5_GetDataSetType(SEXP datasetSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< XPtr<DataSet> >::type dataset(datasetSEXP);
    rcpp_result_gen = Rcpp::wrap(GetDataSetType(dataset));
    return rcpp_result_gen;
END_RCPP
}
// ReadDataset
SEXP ReadDataset(XPtr<DataSet> dataset, XPtr<DataSpace> dataspace, NumericVector count);
RcppExport SEXP h5_ReadDataset(SEXP datasetSEXP, SEXP dataspaceSEXP, SEXP countSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< XPtr<DataSet> >::type dataset(datasetSEXP);
    Rcpp::traits::input_parameter< XPtr<DataSpace> >::type dataspace(dataspaceSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type count(countSEXP);
    rcpp_result_gen = Rcpp::wrap(ReadDataset(dataset, dataspace, count));
    return rcpp_result_gen;
END_RCPP
}
// CloseDataset
bool CloseDataset(XPtr<DataSet> dataset);
RcppExport SEXP h5_CloseDataset(SEXP datasetSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< XPtr<DataSet> >::type dataset(datasetSEXP);
    rcpp_result_gen = Rcpp::wrap(CloseDataset(dataset));
    return rcpp_result_gen;
END_RCPP
}
// CreateDataset
XPtr<DataSet> CreateDataset(XPtr<CommonFG> file, string datasetname, char datatype, NumericVector dimensions, NumericVector chunksize, NumericVector maxshape, int compressionlevel, int size);
RcppExport SEXP h5_CreateDataset(SEXP fileSEXP, SEXP datasetnameSEXP, SEXP datatypeSEXP, SEXP dimensionsSEXP, SEXP chunksizeSEXP, SEXP maxshapeSEXP, SEXP compressionlevelSEXP, SEXP sizeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< XPtr<CommonFG> >::type file(fileSEXP);
    Rcpp::traits::input_parameter< string >::type datasetname(datasetnameSEXP);
    Rcpp::traits::input_parameter< char >::type datatype(datatypeSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type dimensions(dimensionsSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type chunksize(chunksizeSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type maxshape(maxshapeSEXP);
    Rcpp::traits::input_parameter< int >::type compressionlevel(compressionlevelSEXP);
    Rcpp::traits::input_parameter< int >::type size(sizeSEXP);
    rcpp_result_gen = Rcpp::wrap(CreateDataset(file, datasetname, datatype, dimensions, chunksize, maxshape, compressionlevel, size));
    return rcpp_result_gen;
END_RCPP
}
// OpenDataset
XPtr<DataSet> OpenDataset(XPtr<CommonFG> file, string datasetname);
RcppExport SEXP h5_OpenDataset(SEXP fileSEXP, SEXP datasetnameSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< XPtr<CommonFG> >::type file(fileSEXP);
    Rcpp::traits::input_parameter< string >::type datasetname(datasetnameSEXP);
    rcpp_result_gen = Rcpp::wrap(OpenDataset(file, datasetname));
    return rcpp_result_gen;
END_RCPP
}
// GetDataSetDimensions
NumericVector GetDataSetDimensions(XPtr<DataSet> dataset);
RcppExport SEXP h5_GetDataSetDimensions(SEXP datasetSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< XPtr<DataSet> >::type dataset(datasetSEXP);
    rcpp_result_gen = Rcpp::wrap(GetDataSetDimensions(dataset));
    return rcpp_result_gen;
END_RCPP
}
// GetDataSetMaxDimensions
NumericVector GetDataSetMaxDimensions(XPtr<DataSet> dataset);
RcppExport SEXP h5_GetDataSetMaxDimensions(SEXP datasetSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< XPtr<DataSet> >::type dataset(datasetSEXP);
    rcpp_result_gen = Rcpp::wrap(GetDataSetMaxDimensions(dataset));
    return rcpp_result_gen;
END_RCPP
}
// GetDataSetChunksize
NumericVector GetDataSetChunksize(XPtr<DataSet> dataset);
RcppExport SEXP h5_GetDataSetChunksize(SEXP datasetSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< XPtr<DataSet> >::type dataset(datasetSEXP);
    rcpp_result_gen = Rcpp::wrap(GetDataSetChunksize(dataset));
    return rcpp_result_gen;
END_RCPP
}
// GetDataSetCompression
CharacterVector GetDataSetCompression(XPtr<DataSet> dataset);
RcppExport SEXP h5_GetDataSetCompression(SEXP datasetSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< XPtr<DataSet> >::type dataset(datasetSEXP);
    rcpp_result_gen = Rcpp::wrap(GetDataSetCompression(dataset));
    return rcpp_result_gen;
END_RCPP
}
// GetDataspace
XPtr<DataSpace> GetDataspace(XPtr<DataSet> dataset);
RcppExport SEXP h5_GetDataspace(SEXP datasetSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< XPtr<DataSet> >::type dataset(datasetSEXP);
    rcpp_result_gen = Rcpp::wrap(GetDataspace(dataset));
    return rcpp_result_gen;
END_RCPP
}
// SelectHyperslab
XPtr<DataSpace> SelectHyperslab(XPtr<DataSpace> dataspace, NumericVector offset, NumericVector count, string seloper);
RcppExport SEXP h5_SelectHyperslab(SEXP dataspaceSEXP, SEXP offsetSEXP, SEXP countSEXP, SEXP seloperSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< XPtr<DataSpace> >::type dataspace(dataspaceSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type offset(offsetSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type count(countSEXP);
    Rcpp::traits::input_parameter< string >::type seloper(seloperSEXP);
    rcpp_result_gen = Rcpp::wrap(SelectHyperslab(dataspace, offset, count, seloper));
    return rcpp_result_gen;
END_RCPP
}
// SelectElem
XPtr<DataSpace> SelectElem(XPtr<DataSpace> dataspace, NumericMatrix coords);
RcppExport SEXP h5_SelectElem(SEXP dataspaceSEXP, SEXP coordsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< XPtr<DataSpace> >::type dataspace(dataspaceSEXP);
    Rcpp::traits::input_parameter< NumericMatrix >::type coords(coordsSEXP);
    rcpp_result_gen = Rcpp::wrap(SelectElem(dataspace, coords));
    return rcpp_result_gen;
END_RCPP
}
// SelectAll
XPtr<DataSpace> SelectAll(XPtr<DataSpace> dataspace);
RcppExport SEXP h5_SelectAll(SEXP dataspaceSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< XPtr<DataSpace> >::type dataspace(dataspaceSEXP);
    rcpp_result_gen = Rcpp::wrap(SelectAll(dataspace));
    return rcpp_result_gen;
END_RCPP
}
// CloseDataspace
bool CloseDataspace(XPtr<DataSpace> dataspace);
RcppExport SEXP h5_CloseDataspace(SEXP dataspaceSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< XPtr<DataSpace> >::type dataspace(dataspaceSEXP);
    rcpp_result_gen = Rcpp::wrap(CloseDataspace(dataspace));
    return rcpp_result_gen;
END_RCPP
}
// OpenFile
XPtr<H5File> OpenFile(string filePath, string mode);
RcppExport SEXP h5_OpenFile(SEXP filePathSEXP, SEXP modeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< string >::type filePath(filePathSEXP);
    Rcpp::traits::input_parameter< string >::type mode(modeSEXP);
    rcpp_result_gen = Rcpp::wrap(OpenFile(filePath, mode));
    return rcpp_result_gen;
END_RCPP
}
// CloseFile
bool CloseFile(XPtr<H5File> file);
RcppExport SEXP h5_CloseFile(SEXP fileSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< XPtr<H5File> >::type file(fileSEXP);
    rcpp_result_gen = Rcpp::wrap(CloseFile(file));
    return rcpp_result_gen;
END_RCPP
}
// FlushFile
bool FlushFile(XPtr<H5File> file);
RcppExport SEXP h5_FlushFile(SEXP fileSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< XPtr<H5File> >::type file(fileSEXP);
    rcpp_result_gen = Rcpp::wrap(FlushFile(file));
    return rcpp_result_gen;
END_RCPP
}
// IsHDF5File
bool IsHDF5File(string fname);
RcppExport SEXP h5_IsHDF5File(SEXP fnameSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< string >::type fname(fnameSEXP);
    rcpp_result_gen = Rcpp::wrap(IsHDF5File(fname));
    return rcpp_result_gen;
END_RCPP
}
// CreateGroup
XPtr<Group> CreateGroup(XPtr<CommonFG> file, string groupname);
RcppExport SEXP h5_CreateGroup(SEXP fileSEXP, SEXP groupnameSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< XPtr<CommonFG> >::type file(fileSEXP);
    Rcpp::traits::input_parameter< string >::type groupname(groupnameSEXP);
    rcpp_result_gen = Rcpp::wrap(CreateGroup(file, groupname));
    return rcpp_result_gen;
END_RCPP
}
// OpenGroup
XPtr<Group> OpenGroup(XPtr<CommonFG> file, string groupname);
RcppExport SEXP h5_OpenGroup(SEXP fileSEXP, SEXP groupnameSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< XPtr<CommonFG> >::type file(fileSEXP);
    Rcpp::traits::input_parameter< string >::type groupname(groupnameSEXP);
    rcpp_result_gen = Rcpp::wrap(OpenGroup(file, groupname));
    return rcpp_result_gen;
END_RCPP
}
// CloseGroup
bool CloseGroup(XPtr<Group> group);
RcppExport SEXP h5_CloseGroup(SEXP groupSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< XPtr<Group> >::type group(groupSEXP);
    rcpp_result_gen = Rcpp::wrap(CloseGroup(group));
    return rcpp_result_gen;
END_RCPP
}
// ExistsGroup
bool ExistsGroup(XPtr<CommonFG> file, string groupname);
RcppExport SEXP h5_ExistsGroup(SEXP fileSEXP, SEXP groupnameSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< XPtr<CommonFG> >::type file(fileSEXP);
    Rcpp::traits::input_parameter< string >::type groupname(groupnameSEXP);
    rcpp_result_gen = Rcpp::wrap(ExistsGroup(file, groupname));
    return rcpp_result_gen;
END_RCPP
}
// GetGroupNames
CharacterVector GetGroupNames(XPtr<CommonFG> file, string path, bool recursive);
RcppExport SEXP h5_GetGroupNames(SEXP fileSEXP, SEXP pathSEXP, SEXP recursiveSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< XPtr<CommonFG> >::type file(fileSEXP);
    Rcpp::traits::input_parameter< string >::type path(pathSEXP);
    Rcpp::traits::input_parameter< bool >::type recursive(recursiveSEXP);
    rcpp_result_gen = Rcpp::wrap(GetGroupNames(file, path, recursive));
    return rcpp_result_gen;
END_RCPP
}
// GetDataSetNames
CharacterVector GetDataSetNames(XPtr<CommonFG> file, string path, bool recursive);
RcppExport SEXP h5_GetDataSetNames(SEXP fileSEXP, SEXP pathSEXP, SEXP recursiveSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< XPtr<CommonFG> >::type file(fileSEXP);
    Rcpp::traits::input_parameter< string >::type path(pathSEXP);
    Rcpp::traits::input_parameter< bool >::type recursive(recursiveSEXP);
    rcpp_result_gen = Rcpp::wrap(GetDataSetNames(file, path, recursive));
    return rcpp_result_gen;
END_RCPP
}
// GetSoftLinks
CharacterVector GetSoftLinks(XPtr<CommonFG> file, string path);
RcppExport SEXP h5_GetSoftLinks(SEXP fileSEXP, SEXP pathSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< XPtr<CommonFG> >::type file(fileSEXP);
    Rcpp::traits::input_parameter< string >::type path(pathSEXP);
    rcpp_result_gen = Rcpp::wrap(GetSoftLinks(file, path));
    return rcpp_result_gen;
END_RCPP
}
// Unlink
bool Unlink(XPtr<CommonFG> file, string path);
RcppExport SEXP h5_Unlink(SEXP fileSEXP, SEXP pathSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< XPtr<CommonFG> >::type file(fileSEXP);
    Rcpp::traits::input_parameter< string >::type path(pathSEXP);
    rcpp_result_gen = Rcpp::wrap(Unlink(file, path));
    return rcpp_result_gen;
END_RCPP
}
