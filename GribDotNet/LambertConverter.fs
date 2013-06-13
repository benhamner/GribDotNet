module LambertConverter

[<Measure>] type Latitude
[<Measure>] type Longitude

type LambertProjection1 = {
    StandardParallel0 : float<Latitude>
    ReferenceLatitude : float<Latitude>
    ReferenceLongitude : float<Longitude>
    EarthRadius : float
}

// Lambert Conformal Conic Projection 2 Standard Parallels
type LambertProjection2 = {
    StandardParallel1 : float<Latitude>
    StandardParallel2 : float<Latitude>
    ReferenceLatitude : float<Latitude>
    ReferenceLongitude : float<Longitude>
    EarthRadius : float
}

let latitudeDegreesToRadians (latitude:float<Latitude>) = latitude / 180.0<Latitude> * System.Math.PI
let longitudeDegreesToRadians (longitude:float<Longitude>) = longitude / 180.0<Longitude> * System.Math.PI
let radiansToLatitudeDegrees (radians:float) = radians / System.Math.PI * 180.0<Latitude>
let radiansToLongitudeDegrees (radians:float) = radians / System.Math.PI * 180.0<Longitude>

module Inner =
    let boundLongitude (angle:float<Longitude>) =
        let multiple = floor ((angle+180.0<Longitude>) / 360.0<Longitude>)
        let result = angle - multiple*360.0<Longitude>
        result
    let adjustAngle angle =
        let quarterPi = 0.25*System.Math.PI
        0.5 * angle + quarterPi
    let inverseAdjustAngle adjustedAngle =
        (adjustedAngle - 0.25*System.Math.PI) * 2.0
    let toEastingNorthing earthRadius n rho rho0 (referenceLongitude:float<Longitude>) (longitude:float<Longitude>) =
        let theta = n*(longitudeDegreesToRadians (boundLongitude longitude) - longitudeDegreesToRadians (boundLongitude referenceLongitude))
        let easting = rho *sin theta*earthRadius
        let northing = (rho0 - rho*cos theta)*earthRadius
        (easting,northing)

module Inner2 =
    let calculateN (projection:LambertProjection2) =
        let sp1 = latitudeDegreesToRadians projection.StandardParallel1
        let sp2 = latitudeDegreesToRadians projection.StandardParallel2
        let numerator = log(cos sp1 / cos sp2)
        let denominator = log(tan(Inner.adjustAngle sp2)/tan(Inner.adjustAngle sp1))
        let result = numerator/denominator
        result
    let calculateF (projection:LambertProjection2) n =
        let sp1 = latitudeDegreesToRadians projection.StandardParallel1
        cos sp1 * (tan(Inner.adjustAngle sp1) ** n) / n
    let calculateRho (projection:LambertProjection2) n (latitude:float<Latitude>) =
        let angle = latitudeDegreesToRadians latitude
        (calculateF projection n) / (tan(Inner.adjustAngle angle) ** n)
    let inverseRho (projection:LambertProjection2) n rho =
        let tanAngle = ((calculateF projection n) / rho) ** (1.0/n)
        let result = Inner.inverseAdjustAngle (atan tanAngle) |> radiansToLatitudeDegrees
        result

let fromLambert2 (projection:LambertProjection2) (coordinates:float*float) =
    let (easting,northing) = coordinates
    let n = Inner2.calculateN projection
    let rho0 = Inner2.calculateRho projection n (projection.ReferenceLatitude)
    let coNorthing = rho0 - (northing/projection.EarthRadius)
    let scaledEasting = easting/projection.EarthRadius
    let theta = atan (scaledEasting / coNorthing)
    let rho = sqrt (scaledEasting*scaledEasting + coNorthing*coNorthing) * float(sign n)
    let latitude = Inner2.inverseRho projection n rho
    let longitude = projection.ReferenceLongitude + radiansToLongitudeDegrees (theta/n)
    (latitude,longitude)

let toLambert2 (projection:LambertProjection2) (coordinates:float<Latitude>*float<Longitude>) =
    let (latitude,longitude) = coordinates
    let n = Inner2.calculateN projection
    let rho = Inner2.calculateRho projection n latitude
    let rho0 = Inner2.calculateRho projection n (projection.ReferenceLatitude)
    let result = Inner.toEastingNorthing (projection.EarthRadius) n rho rho0 (projection.ReferenceLongitude) longitude
    result

module Inner1 =
    let calculateN (projection:LambertProjection1) =
        let sp1 = latitudeDegreesToRadians projection.StandardParallel0
        let result = sin sp1
        result
    let calculateRho (projection:LambertProjection1) n (latitude:float<Latitude>) =
        let sp1 = latitudeDegreesToRadians projection.StandardParallel0
        let angle = latitudeDegreesToRadians latitude
        let result = ((tan (Inner.adjustAngle sp1)/ tan(Inner.adjustAngle angle)) ** n) * cos sp1 / n
        result
    let inverseRho (projection:LambertProjection1) n rho =
        let sp1 = latitudeDegreesToRadians projection.StandardParallel0
        let tanAngle = (tan (Inner.adjustAngle sp1)) / ((rho * n / cos sp1) ** (1.0/n))
        let result = Inner.inverseAdjustAngle (atan tanAngle) |> radiansToLatitudeDegrees
        result

let toLambert1 (projection:LambertProjection1) (coordinates:float<Latitude>*float<Longitude>) =
    let (latitude,longitude) = coordinates
    let n = Inner1.calculateN projection
    let rho = Inner1.calculateRho projection n latitude
    let rho0 = Inner1.calculateRho projection n (projection.ReferenceLatitude)
    let result = Inner.toEastingNorthing (projection.EarthRadius) n rho rho0 (projection.ReferenceLongitude) longitude
    result

let fromLambert1 (projection:LambertProjection1) (coordinates:float*float) =
    let (easting,northing) = coordinates
    let n = Inner1.calculateN projection
    let rho0 = Inner1.calculateRho projection n (projection.ReferenceLatitude)
    let coNorthing = rho0 - (northing/projection.EarthRadius)
    let scaledEasting = easting/projection.EarthRadius
    let theta = atan (scaledEasting / coNorthing)
    let rho = sqrt (scaledEasting*scaledEasting + coNorthing*coNorthing) * float(sign n)
    let latitude = Inner1.inverseRho projection n rho
    let longitude = projection.ReferenceLongitude + radiansToLongitudeDegrees (theta/n)
    (latitude,longitude)