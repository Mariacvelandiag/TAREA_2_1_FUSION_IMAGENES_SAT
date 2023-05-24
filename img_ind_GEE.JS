//--------------------------------------------------------------------------
//------- CÓDIGO_1 Tarea2_parte1 -------------------------------------------
//------- Vision Artificial 1-----------------------------------------------
//------- Por: Maria Camila Velandia Garcia---------------------------------
//-------      mariac.velandiag@udea.edu.co --------------------------------
//-------      Estudiante Maestría en Ingeniería  --------------------------
//-------      CC 1017252095, Wpp 3117318160 -------------------------------
//------- Jueves 25 mayo de 2023--------------------------------------------
//--------------------------------------------------------------------------

//--------------------------------------------------------------------------
//--1. Inicializo el sistema -----------------------------------------------
//--------------------------------------------------------------------------

//Se tiene una region de interes en el Meta, especificamente Rey Zamuro, donde se localizaron alrededor de 105 grabadoras
//-----CARGAR ZONA DE INTERES
//AGREGAR "ACA" SHAPE CREADO DE LA CARPETA "Poligono"
Map.addLayer(roi,{},"Polígono Rey Zamuro", true);
print(roi)
//-----Cálculo del área del poligono a trabajar la imagen satelital
var area = roi.geometry().area().divide(1000 * 1000); // Dividir por 1000 para convertir a km^2
print ("Superficie:",area, "km^2");
//-----Cálculo del Perímetro del poligono a trabajar la imagen satelital
var perimeter = roi.geometry().perimeter();
print("Perimetro:", perimeter.divide(1000), "km");

//--------------------------------------------------------------------------
//--2. Preprocesamiento de la imagen ---------------------------------------
//--------------------------------------------------------------------------
//-----Corrección topográfica
//------Filtro de nubes y cirros

function cloud_mask(image) {
  var qa = image.select('QA60');
  var cloudBitMask = 1 << 10;
    var cirrusBitMask = 1 << 11;
  var mask = qa.bitwiseAnd(cloudBitMask).eq(0).and(
             qa.bitwiseAnd(cirrusBitMask).eq(0));
  var out = image.updateMask(mask).divide(10000)
      .select("B.*")
      .copyProperties(image);
  return out;
  }
  
//-----Modelo de elevación digital
//------Para que los valores de los pixeles se adapte al relieve
//------En otras palabras, es una representación matemática de cómo
//------varía la altura o elevación de un área específica del terreno
var dem = ee.Image("USGS/SRTMGL1_003");

//--------------------------------------------------------------------------
//--funciones de filtrado --------------------------------------------------
//--------------------------------------------------------------------------
function topoCorr_IC(img){

  // Extraer metadatos de la imagen sobre la posición solar(CORRECION DEL ANGULO DEL SENSOR DEL SATELITE)
  var SZ_rad = ee.Image.constant(ee.Number(img.get('MEAN_SOLAR_ZENITH_ANGLE'))).multiply(3.14159265359).divide(180).clip(img.geometry().buffer(10000)); 
  var SA_rad = ee.Image.constant(ee.Number(img.get('MEAN_SOLAR_AZIMUTH_ANGLE')).multiply(3.14159265359).divide(180)).clip(img.geometry().buffer(10000)); 
  // Crear capas de terreno
  var slp = ee.Terrain.slope(dem).clip(img.geometry().buffer(10000));
  var slp_rad = ee.Terrain.slope(dem).multiply(3.14159265359).divide(180).clip(img.geometry().buffer(10000));
  var asp_rad = ee.Terrain.aspect(dem).multiply(3.14159265359).divide(180).clip(img.geometry().buffer(10000));

  // Calcular la condición de iluminación (IC)
  // parte de pendiente de la condición de iluminación
  var cosZ = SZ_rad.cos();
  var cosS = slp_rad.cos();
  var slope_illumination = cosS.expression("cosZ * cosS", 
                                          {'cosZ': cosZ,
                                           'cosS': cosS.select('slope')});
  // Parte de aspecto de la condición de iluminación
  var sinZ = SZ_rad.sin(); 
  var sinS = slp_rad.sin();
  var cosAziDiff = (SA_rad.subtract(asp_rad)).cos();
  var aspect_illumination = sinZ.expression("sinZ * sinS * cosAziDiff", 
                                           {'sinZ': sinZ,
                                            'sinS': sinS,
                                            'cosAziDiff': cosAziDiff});
  // Condición de plena iluminación (IC)
  var ic = slope_illumination.add(aspect_illumination);

  // Añadir IC a la imagen original
  var img_plus_ic = ee.Image(img.addBands(ic.rename('IC')).addBands(cosZ.rename('cosZ')).addBands(cosS.rename('cosS')).addBands(slp.rename('slope')));
  return img_plus_ic;
}

function topoCorr_SCSc(img){
    var img_plus_ic = img;
    var mask1 = img_plus_ic.select('B8').gt(-0.1);
    var mask2 = img_plus_ic.select('slope').gte(5) //se aplican a terrenos con una inclinación superior a 5 grados
                            .and(img_plus_ic.select('IC').gte(0));
    var img_plus_ic_mask2 = ee.Image(img_plus_ic.updateMask(mask2));

    var bandList = ['B1','B2', 'B3', 'B4', 'B5', 'B6', 'B7', 'B8', 'B9','B11', 'B12']; // Specify Bands to topographically correct

    function apply_SCSccorr(bandList){
      var method = 'SCSc';
      var out = img_plus_ic_mask2.select('IC', bandList).reduceRegion({
      reducer: ee.Reducer.linearFit(), // Calcular coeficientes: a(slope), b(offset), c(b/a)
      geometry: ee.Geometry(img.geometry().buffer(-5000)), // recortar los bordes exteriores de la imagen para obtener una relación lineal 
      scale: 500,
      maxPixels: 10e13
      }); 
      var out_a = ee.Number(out.get('scale'));
      var out_b = ee.Number(out.get('offset'));
      // out_a and out_b pueden ser nulos dando error al calcular out_c
      // var out_c = ee.Number(out.get('offset')).divide(ee.Number(out.get('scale')));      
      var out_c = ee.Algorithms.If(
        out_a, 
        ee.Image(ee.Number(out.get('offset')).divide(ee.Number(out.get('scale')))),
        ee.Image() // Imagen enmascarada
      )

      //Aplicar la corrección SCSc
      var SCSc_output = img_plus_ic_mask2.expression("((image * (cosB * cosZ + cvalue)) / (ic + cvalue))", {
        'image': img_plus_ic_mask2.select(bandList),
        'ic': img_plus_ic_mask2.select('IC'),
        'cosB': img_plus_ic_mask2.select('cosS'),
        'cosZ': img_plus_ic_mask2.select('cosZ'),
        'cvalue': out_c
      });

      return ee.Image(SCSc_output);
    }

    var img_SCSccorr = ee.Image(bandList.map(apply_SCSccorr)).addBands(img_plus_ic.select('IC'));
    var bandList_IC = ee.List([bandList, 'IC']).flatten();
    return img_SCSccorr.unmask(img_plus_ic.select(bandList_IC))
                       .addBands(mask1.rename('initMask'))
                       .addBands(mask2.rename('corrMask'));
  }

//--------------------------------------------------------------------------
//SELECCION DE LA IMAGEN, CON REGION DE INTERES, RESOLUCION TEMPORAL, FILTRADO EXTRAIDO DEL SATELITE DE PREFERENCIA
//EN ESTE CASO SENTINEL 2 SENSOR: MSI---------------------------------------
//--------------------------------------------------------------------------
var S2_original = ee.ImageCollection('COPERNICUS/S2_SR_HARMONIZED')
                    .filterBounds(roi)
                    .filterDate("2022-01-01", "2023-01-31")
                    .filterMetadata('MEAN_SOLAR_ZENITH_ANGLE', 'less_than', 85)
                    .map(cloud_mask);
                    
//--------------------------------------------------------------------------
//FILTRADO DE LA MEDIANA:
var S2_topo_corr = S2_original
                    .map(topoCorr_IC) 
                    .map(topoCorr_SCSc);

var mediansentinel = S2_topo_corr.median();
//var S2_corregida= S2_topo_corr.select(['B2', 'B3', 'B4', 'B5', 'B8', 'B9','B11', 'B12'])

//---Visualizacion de la imagen original, antes de la eliminacion de nubes
Map.addLayer(
              S2_original.select(['B4','B3','B2'])
              .reduce(ee.Reducer.percentile([25])).clip(roi),
              {min:0,max:0.2},
              'Original', true
);
//---Visualizacion de la imagen con eliminacion de nubes
Map.addLayer(
              S2_topo_corr.select(['B4','B3','B2'])
              .reduce(ee.Reducer.percentile([25])).clip(roi),
              {min:0,max:0.2},
              'Corrected', true
)  

Export.image.toDrive({image:mediansentinel, description:"sentinel-2topo_corr_veor_Har", region: roi,scale: 10, maxPixels: 1600000000})

var sentinel=mediansentinel

//--------------------------------------------------------------------------
//--3. Procesamiento imagen "lista(corregida)"------------------------------
//--------------------------------------------------------------------------
//-----Cálculo de Características (índices satelitales)
 
//--------NDBI: índice de construcción de diferencia normalizada utiliza las bandas NIR y SWIR para enfatizar las áreas construidas fabricadas. 
var NDBI = sentinel.normalizedDifference(['B11', 'B8']).clip(roi)
Map.addLayer(NDBI,{},"NDBI")
//--------NDMI: El índice de humedad se utiliza para determinar el contenido de agua de la vegetación
var NDMI = sentinel.normalizedDifference(['B8', 'B11']).clip(roi)
Map.addLayer(NDBI,{},"NDMI")
//--------MNDWI: índice de agua de diferencia normalizada modificado utiliza bandas verdes y SWIR para mejorar las características de las aguas abiertas
var MNDWI= sentinel.normalizedDifference(['B3', 'B11']).clip(roi)
Map.addLayer(MNDWI, {}, "MNDWI")
//--------SLAVI: Índice de Vegetación de Área Foliar Específica
var slavi =sentinel.expression("(WV)/(RE1 + SWIR2)", {
  "WV":sentinel.select('B9'),
  "RE1":sentinel.select('B5'),
  "SWIR2":sentinel.select('B12')}).clip(roi);
Map.addLayer(slavi, {}, "SLAVI")
//--------EVI: Índice de Vegetación Mejorado, similar alÍndice de vegetación de diferencia normalizada(NDVI) y se puede utilizar para cuantificar el verdor de la vegetación 
var EVI = sentinel.expression("F*((NIR - R)/(NIR + 6*R - 7.5*B + 1))", {
  "F":2.5,
  "NIR":sentinel.select('B8'),
   "R":sentinel.select('B4'),
   "B":sentinel.select('B2')}).clip(roi); 
Map.addLayer(EVI,{},"EVI")
//--------BRILLO
var Brillo = sentinel.expression ("0.3037*B + 0.2793*G + 0.4743*R + 0.5585*NIR + 0.5082*WV + 0.1863*SWIR",{
  "B":sentinel.select('B2'),
  "G":sentinel.select('B3'),
  "R":sentinel.select('B4'),
  "NIR":sentinel.select('B8'),
  "WV":sentinel.select('B9'),
  "SWIR":sentinel.select('B12')}).clip(roi);
Map.addLayer(Brillo,{},"Brillo")  
//--------NDRE:  El índice de borde rojo de diferencia normalizada (NDRE para abreviar) es un método para medir la cantidad de clorofila en las plantas
var NDRE= sentinel.normalizedDifference(['B8', 'B5']).clip(roi)
Map.addLayer(NDRE,{},"NDRE")
//-------- NDVI: utiliza para cuantificar el verdor de la vegetación y es útil para comprender la densidad de la vegetación y evaluar los cambios en la salud de las plantas
var NDVI = sentinel.normalizedDifference(['B8', 'B4']).clip(roi)
Map.addLayer(NDVI,{},"NDVI")
//---CENTRAR LA IMAGEN AL PUNTO Y ZOOM DESEADO
Map.addLayer(roi)
Map.centerObject(roi,10)

//---Descarga de imagen satelital con carateristicas extraidas

// Export.image.toDrive({image:sentinel, description:"Sentinel_VA2", region:roi,scale: 10, maxPixels: 1600000000})
// Export.image.toDrive({image:NDRE, description:"NDRE_VA2", region:roi,scale: 10, maxPixels: 1600000000})
// Export.image.toDrive({image:EVI, description:"EVI_VA2", region:roi,scale: 10, maxPixels: 1600000000})
// Export.image.toDrive({image:slavi, description:"SLAVI_VA2", region:roi,scale: 10, maxPixels: 1600000000})
// Export.image.toDrive({image:MNDWI, description:"MNDWI_VA2", region:roi,scale: 10, maxPixels: 1600000000})
// Export.image.toDrive({image:NDBI, description:"NDBI_VA2", region:roi,scale: 10, maxPixels: 1600000000})
// Export.image.toDrive({image:Brillo, description:"Brillo_VA2", region:roi,scale: 10, maxPixels: 1600000000})
// Export.image.toDrive({image:NDVI, description:"NDVI_VA2", region:roi,scale: 10, maxPixels: 1600000000})
  
