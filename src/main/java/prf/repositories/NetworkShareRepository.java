package prf.repositories;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import prf.entities.NetworkShare;

@Repository
public interface NetworkShareRepository extends JpaRepository<NetworkShare, Long>{

}
