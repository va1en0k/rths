struct ray
{
    float3 origin;
    float3 direction;
};

struct hit
{
    float t;
    float3 point;
    float3 normal;
    float sphereId;
};

struct sphere
{
    float3 center;
    float radius;
};


bool sphereHit(struct sphere s, struct ray r, float from, float to, struct hit * hit) {
  float3 oc = r.origin - s.center;
  float a = dot(r.direction, r.direction);
  float b = 2.0 * dot(oc, r.direction);
  float c = dot(oc, oc) - s.radius * s.radius;
  float dsc = b * b - 4.0 * a * c;

  /*hit->t = s.radius;
  hit->point = s.center;
  hit->normal = r.direction;
  return true;*/

  if (dsc < 0.0) {
    //hit->t = 0;
    //hit->point = (float3)(0, 0, 0);
    //hit->normal = (float3)(0, 0, 0);
    return false;
  }
  float sqDsc = sqrt(dsc);
  float x = (- b - sqDsc) / (2.0 * a);
  if (!(x >= from && x <= to)) {
    x = (- b + sqDsc) / (2.0 * a);
  }
  if (!(x >= from && x <= to)) {
    return false;
  }
  hit->t = x;
  hit->point = r.origin + x * r.direction;
  hit->normal = (hit->point - s.center) / s.radius;

  return true;
}

kernel void doubleArray(
    global float8 *frayIn,
    global float4 *allSpheres,
    global float8 *fout
) {

    int i = get_global_id(0);

    int outId = i;
/*        fout[outId] = (float8)(
      i,
      sphereId,
      get_global_size(0),get_global_size(1),-1,-1,-1,-1
    );
    return;*/

    struct ray rayIn = {
      {frayIn[i][0], frayIn[i][1], frayIn[i][2]},
      {frayIn[i][3], frayIn[i][4], frayIn[i][5]}
    };

    //int sphereId = get_global_id(1);
    int sphereCount = get_global_size(1);

    struct hit h, hbest;
    hbest.t = 1; //lol random

    for (int sphereId = 0; sphereId <= 2116; sphereId++) {
      struct sphere sphere = {
        {allSpheres[sphereId][0], allSpheres[sphereId][1], allSpheres[sphereId][2]},
        allSpheres[sphereId][3]
      };

      //struct sphere s = {{0.1, 0.1, 0.1}, 1.1};


      if (sphereHit(sphere, rayIn, 0.00001, hbest.t, &hbest)) {
        /*if (hbest.t > h.t) {
          hbest = h;
        }*/
        hbest.sphereId = sphereId;
      }
    }
    if (hbest.t == 100000) hbest.t = -1;

    /*struct hit h = {
      rayIn.direction.x + rayIn.direction.y/10,
      {rayIn.direction.y + rayIn.direction.z/10,4,5},
      {1,2,3}
    };*/

    fout[outId] = (float8)(
      hbest.t,
      hbest.point[0], hbest.point[1], hbest.point[2],
      hbest.normal[0], hbest.normal[1], hbest.normal[2],
      hbest.sphereId
    );
}
